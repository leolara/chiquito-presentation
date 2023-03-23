# DRAFT: A simple high-level step-based language for plonkish circuits

Codename: Chiquito pecador plonkish
Project Goal: Halo2/plonkish circuits for the average programmer
Next step: demo on last week of residency

The abstraction of steps is very useful to build a zk system that proves the execution of a VM. In the current state of zk systems, circuits are akin of [Combinational](https://en.wikipedia.org/wiki/Combinational_logic), zk circuits that would be akin [Sequential](https://en.wikipedia.org/wiki/Sequential_logic) would require more advancement on recursive or aggregated proofs.

This is because the steps abstraction allows us to flatten the execution that is being proven, by putting different steps of the execution in different rows, and using the columns as signals for the circuit that constraint that step, and then using rotations to constrain the transition from one execution step into another.

However, it is not as simple when steps of execution have very different complexities, like the EVM does, this can be seen as a CISC type VM. A solution could be to translate the VM instructions to a micro-VM is that RISC, but this has other problems.

The abstraction proposed here is a generalisation of what is being done in the zkevm-ce to prove the execution of EVM steps, to prove the bytecode contents of a smartcontract, proving MPT or EVM transitions state . It abstract concepts of CellManager, Steps, Steps selector, Constrain Builder and other abstractions in order to allow to have steps of execution proves that take different number of rows and columns. 

Additionally, it is necessary to developer more level abstractions to describe circuits in order to improve the productivity of circuit developers. 

Hence, the main idea is to develop a higher level of abstraction language based on this generalisation. This will allow software engineers to learn to write circuits faster, and to experienced circuit developers to focus on the what instead of the how.

![[Howwhat.svg]]


## Rotations between steps

If each step has the same number of rows `h` we can have a type of SuperRotation between the steps, where the plonkish prover rotation is equal to the base rotation of the signal in the step plus the number of steps we want to rotate over multiplied by `h`.

![[SuperColumns.svg]]

It is more interesting for us the case when the steps has different heights. It is not possible to do what we have shown, but a more restricted version of this is possible. In each step we can know the height of the SuperRow of that step, if some cells are common between steps and are aligned at the top, we can query same cell of exactly the next step. Likewise, if some cells are common between steps and are aligned at the bottom.

![[Forward and backward signals.svg]]

## Terminology

Plonkish machine, a columns and rows based proving system based on polynomials of arbitrary degree.

Step based circuit is made of steps that are activated sequentially for different set of rows and columns. Each step can take a different number of rows and columns. But the rows are sequential. It compiles to a plonkish machine.

Step type, a step template that can be instanced several times.

Step type definition, a set of constrains that will apply to instances of a step type.

Step instance, each instance of activated a step category in the circuit.

SuperRow/Step region/VirtualRow, rows that correspond to a Step instance.

(Internal) Signal/Cell, a witness value that can only be part of constrains of a particular step.

SuperColumn, a signal that is shared across all steps, and that can be queried (SuperRotation) in other steps. To achieve this, all steps should have the same number of rows, which is not ideal.

Forward Signal/Cell, a signal that is shared across all steps, and constrains can rotate to the next step

Forward rotation, query of a forward signal in the next SuperRow.

Internal constraints, constrains that only include signals without rotations.

Forward Constraints/Transition, constrains that include signals with rotations.

Cell Manager abstracts transparently the placement of signals/cells in the plonkish machine for each step category relatively to the step instance start row/column.

Step Selector Manager abstracts transparently how each instance step is activated in each SuperRow.

## A high-level abstraction

We can express step based circuits in this way:

```
StepCircuit C {
	forward a;
	forward b;
	forward c;

	step_type s1 {
		signal d, e, f, ;
		
		cond( d+f == a);
		cond( f*z == b + e );
		
		transition( next(c) + f == -1 );
		transition( next(b) == 14 );
		
		wg (input) {
			a = ...;
			b = ...;
			c = ...;
			d = ...;
			f = ...;
			g = ...;
		}
	}

	step_type s2 {
		signal r, t, v;
		// ...
	}

	trace(input) {
		for line in input {
		   if line.R == 0 {
			   add s1(line)
		   } else {
			   add s2(line)
		   } 
		}
	}
}
```

We define the forward signals in the circuit:

```
forward a;
forward b;
forward c;
```

All steps instances will have an instance of these signals, will able to query them and must assign them in the witness generation, also can query these signals in the next step.

Then we define step category s1, and its internal signals:
```
step s1 {
		signal d, f, g;
```

Each instance of this step will have instances of these signals, but no rotation can be done to them.

We define the internal constraints that are applied in the instances of step `s1`:

```
cond( d+f == a);
cond( f*z == b + g );
```

These constrains cannot include super rotations.

There is additionally the definition of transition constrains, that should contain SuperRotations:

```
transition( next(c) + f == -1 );
transition( next(b) == 14 );
```

Then we code the witness generation of the step, that must generate the values for each step instance for all forward and internal signals.

```
wg (input) {
	a = ...;
	b = ...;
	c = ...;
	d = ...;
	f = ...;
	g = ...;
}
```

We can add as many step categories as we need:

```
step s2 {
	signal r, t, v;
	// ...
}
```

In the global circuit we generate the witness:

```
wg(input) {
	for line in input {
	   if line.R == 0 {
		   add s1(line)
	   } else {
		   add s2(line)
	   } 
	}
}
```

The witness is created by adding step instances in order, the SuperRows are created one after the other and the witness of each SuperRow is filled by the witness generation code of each step definition.

## A plonkish abstraction

### Developer view

![[Developer abstraction.svg]]


### Plonkish arithmetization


![[Plonkish arith.svg]]


## Real life example: Bytecode circuit

https://github.com/privacy-scaling-explorations/zkevm-circuits/blob/main/zkevm-circuits/src/bytecode_circuit/circuit.rs


```
Circuit bytecode(challenge_keccak, push_data_size_table, keccak256_table) {
	forward length;
	forward index;
	forward hash;
	forward value_rlc;

	step_type header() {
		cond(index == 0);
		cond(value == length);

		transition_next(header)(
			length == 0 AND
			hash == EMPTY_HASH
		)
		transition_next(byte)(
			length == next(length) AND
			next(index) == 0 AND
			next(is_code) == 1 AND
			next(hash) == hash AND
			next(value_rlc) == next(value)
		)

		wg(length, hash) {
			index := 0;
			length := length
			value := length
			hash:= hash
			value_rlc := 0 // could be anything
		}
	}

	step_type byte() {
		signal push_data_size;
		signal push_data_left_inv

		gadget push_data_left_is_zero = isZero.new(push_data_left, push_data_left_inv);
		
		cond(is_code == push_data_left_is_zero.is_zero)

		lookup(
			(value, push_data_size_table.value)
			(push_data_size, push_data_size_table.push_data_size)
		)

		transition_if_next_step(byte)(
			next(length) == length AND
			next(index) == index + 1 AND
			next(hash) == hash AND
			next(value_rlc) == value_rlc * challenge_keccak + next(value) AND
			next(push_data_left) == is_code ? push_data_size : push_data_left - 1
		)

		transition_next(header)(
			index + 1 == length
		)

		transition_next(header)(
			lookup(
				(value_rlc, keccak256_table.value_rlc),
				(length, keccak256_table.length),
				(hash, keccak256_table.hash),
			)
		)
		
		wg(bytecode, value_rlc, push_data_size) {
			isZero.wg(bytecode.push_data_left, push_data_left_inv)

			value := bytecode.value
			index := bytecode.index
			hash := bytecode.hash
			length := bytecode.length
			
			value_rlc := value_rlc
			push_data_size := push_data_size
		}
	}

	trace(bytcodes bytecode[]) {
		for each bytecodes as bytecode {
			add header(bytecode.length, bytecode.hash)
			var value_rlc = 0
			
			for each bytecode as byte {
				value_rlc = value_rlc * keccak_challenge + byte.value
				add byte(bytecode, value_rlc, get_push_size(value))
			}
		}
	}
}
```

## Posible implementation stacks

+ Rust DSL (embedded)
+ Rust DSL with macros (embedded)
+ Python/typescript/ruby DSL that compiles to rust
+ Custom parser that compiles to rust or embedded in rust ()

## Implementation as rust DSL
```rust
use chiquito::{
    ast::{ToExpr, ToField},
    dsl::circuit,
};
use halo2_proofs::halo2curves::bn256::Fr;

fn main() {
    circuit::<Fr, Vec<i32>, i32, _>("a circuit", |ctx| {
        let a = ctx.forward("a");
        let b = ctx.forward("b");
        let c = ctx.forward("c");

        let s1 = ctx.step_type("s1", |ctx| {
            let d = ctx.signal("d");
            let f = ctx.signal("f");

            ctx.cond("annotation", 0.into());
            ctx.transition("annotation", a.expr() + 1);

            ctx.wg(move |ctx, _| {
                ctx.assign(a, 13.field());
                ctx.assign(b, 13.field());
                ctx.assign(c, 13.field());

                ctx.assign(d, 1.field());
                ctx.assign(f, 2.field());
            });
        });

        let s2 = ctx.step_type("s2", |ctx| {
            // ...

            ctx.wg(move |ctx, _| {
                ctx.assign(a, 13.field());
            })
        });

        ctx.trace(move |ctx, _| {
            // ...
            let v: i32 = 1;
            ctx.add(&s2, v);
            ctx.add(&s1, v);
        });
    });
}

```

## Advantages
 + Readability
 + Abstracts transparently CellManager and the placement of steps and signals (SuperRows). You can try different strategies and configurations.
 + Encapsulation of constrains and witness generation, but separating them.
 + Abstract transparently the selector of steps.
 + Abstract transparently the rotations to query values in other steps (SuperRotations).

## Ideas for further improvements

### Nesting

We could create steps that are compose of step based circuits, this would make the language much more powerful. How SuperRotations are done in this case, is an interesting problem.

### Composition and lookup tables

TBD

### Witness inference

Sometimes it is possible to infer how to assign some signals from constrains. This is for example what circom does with `<==`. This can cause some security problems like leaking information.

If this is done, it should be more explicit for the developer what is going on:

```
step ... {
	cond(c == a + b)
	...
	wg() {
		a := 2
		b := 2
		infer(c)
	}
}
```

## Questions / feedback


