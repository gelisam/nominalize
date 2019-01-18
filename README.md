# generics-unrep

(work in progress: this Readme documents the aspirations of this package, not its current status)

Generate types using type-generic programming, retaining control over the names of the constructors and the fields.

# Generating types

Type-generic programming ([GHC.Generics](http://hackage.haskell.org/package/base/docs/GHC-Generics.html), [generics-sop](http://hackage.haskell.org/package/generics-sop), [generics-eot](http://hackage.haskell.org/package/generics-eot), etc.) makes it easy to generate functions (`(==)`, `toJSON`, `diff`, `toDbEncoding`, etc.) for a given source type. It is also easy to generate associated types (`Patch`, `DbEncoding`, etc.) from a source type. However, since type-generic programming works by converting the source type into an isomorphic generic representation based on simple sums and products, those generated associated types will often also be based on simple sums and products, and will only be isomorphic to the target type we would like to generate. In particular, the names of its constructors and fields are likely to look like `LeftPatch` or `dbField1` instead of something more relevant to the original source type, such as `LeafPatch` or `dbLeftSubTree`. This library aims to fix that.

When using `GHC.Generics`, the generic representation is a more complicated than just simple sums and products, the generic representation also includes some metadata about the name of the type, the name of its constructors, the name of its fields, and more details such as whether each field is strict or not. This is useful when generating functions like `toJSON`, to generate json objects whose fields correspond to the names of the source type's fields. The key insight of this library is that we can also use this metadata to generate associated types which have their own metadata about which names the target type should use. On its own, this isn't yet useful because the generated type will still have inscrutable constructor names such as `M1`, all that metadata is only at the type level. The final step is to call the TemplateHaskell function provided by this library, which reads this metadata and creates the corresponding datatype.

So, if TemplateHaskell allows us to generate datatypes with the names we want, why not use TemplateHaskell instead of this library? Well, if you like to use TemplateHaskell, feel free to do that, but in my experience writing type-generic programming code has been a lot easier (though definitely not _easy_) than writing TemplateHaskell code, so really, I'm writing the TemplateHaskell code so you don't have to :)