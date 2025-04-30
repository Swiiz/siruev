use siruev::*;

#[derive(Event, Debug)]
struct EventA<'a>(&'a str);

#[derive(Event, Debug)]
#[allow(dead_code)]
struct EventB(&'static str);

#[event_handler]
fn on_event1(event: &EventA) {
    println!("on_event1: {}", event.0);
}

#[event_handler(priority = -1, &EventA, &EventB)]
fn on_event2<'a, T: std::fmt::Debug>(event: &T) {
    println!("on_event2: {:?}", event);
}

#[event_handler]
fn on_event3(event: &mut EventA) {
    println!("on_event3: {}", event.0);
    event.0 = &event.0[5..];
}

fn main() {
    emit(&EventA("hello"));
    emit(&EventB("world"));

    let mut event = EventA("to be changed");
    emit_mut(&mut event);
    println!("{}", event.0);
}
