## 13. **Processing JSON** using Circe. Custom coders and decoders

### Parsing JSON from NBA API

Some classes and generated JSON codecs are provided for NBA API. Unfortunately, they don't work as expected out of the box. The task is to fix (rewrite) some of the codecs to make tests pass.

- You are not supposed to change anything in _class_ HomeworkSpec, instead of it you are supposed to keep your changes inside _companion object_ for HomeworkSpec.

- You are not allowed to rename fields in case classes.

- You are not allowed to remove fields from case classes.

- You are supposed to use camelCase if you introduce additional fields in case classes.


It would be nice to avoid using Encoder/Decoder.forProductN where you specify all field names

