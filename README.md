# SIRUEV - **Si**mple **Ru**st **Ev**ents ([Docs](https://swiiz.github.io/siruev))

`siruev` is a simple yet powerful event-system for Rust that enables you to define and handle events
in a modular way, allowing for minimal coupling. It supports any event handler type and allows the user
to emit any event type even with lifetime in order to trigger these handlers. Additionally, it features
parallel execution support, enabling concurrent handler invocations if the `parallel` feature is enabled.

## Features
- **Event Handling**: Define and register event handlers with minimal boilerplate.
- **Event Lifetime**: Support for events with arbitrary lifetime. (*one at most right now*)
- **Emit Events**: Dispatch events to registered handlers with ease.
- **Parallel Execution**: Optionally execute handlers concurrently with the `parallel` feature enabled.
- **Priority Handling**: Handlers can be define a `priority` value per event type, allowing control over their invocation order.

## Example Usage

This example demonstrates how to define events, handle them, and emit them:
For more advanced usage, check out the examples folder or refer to the detailed documentation.

```rust
use siruev::*;

#[derive(Event, Debug)]
struct MyEvent(String);

#[event_handler]
fn on_event(event: &MyEvent) {
    println!("Event received: {}", event.0);
}

#[event_handler]
fn on_event_mut(event: &mut MyEvent) {
    println!("Mut event received: {}", event.0);
}

fn main() {
    let event = MyEvent("Hello, world!".to_string());

    // Dispatches the event to all handlers registered for
    emit(&event); // `&MyEvent`
    emit_mut(&mut event); // `&mut MyEvent`
}
```

## Key Components

- **[`crate::Event`] trait**: All event types must implement this trait, which defines the lifetime bounds and the system registration.
- **[`#[event_handler]`](crate::event_handler) macro**: Registers functions to handle specific event types.
- **[`crate::emit`] and [`crate::emit_mut`] functions**: Dispatch events to registered handlers. `emit` is for immutable events, while `emit_mut` is for mutable events.

## Features Flag

- **Parallel Feature**: The `parallel` feature enables concurrent execution of event handlers via the `rayon` library. Handlers can be executed in parallel when this feature is enabled, improving performance for large numbers of handlers.

## Example with Parallel Feature
If the `parallel` feature is enabled, the handlers for events will be invoked concurrently:

```rust
emit(MyEvent("Parallel execution".to_string())); // Handlers may execute concurrently with rayon
```

## Todo:
- Threadlocal event handlers?
