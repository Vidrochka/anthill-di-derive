# anthill-di-derive
```anthill-di``` derive extensopn

## Basic concepts

Add ```#[derive(constructor)]``` on top of struct

``` rust
#[derive(constructor)]
struct TestInjection {}
```

Register dependency

``` rust
#[derive(constructor)]
struct TestInjection1 {
    // simple resolve call (can be omitted)
    #[resolve] TestInjection2
    
    // resolve custom data
    #[custom_resolve(value = "\"test3\".to_string()")] str: String,

    // save context
    #[ioc_context] di_context: anthill_di::DependencyContext,

    // resolve collection of service
    #[resolve_collection] collection: Vec<Box<dyn GetStr>>,

    // resolve service by component type
    #[resolve_by_component(TestInjection3)] second: Box<dyn GetStr>,
}
```

Crate version is equal minimal required version of ```anthill-di```

Crate required dependency:

* [anthill-di](https://github.com/Vidrochka/anthill-di)
* [async-trait-with-sync](https://crates.io/crates/async-trait-with-sync)
