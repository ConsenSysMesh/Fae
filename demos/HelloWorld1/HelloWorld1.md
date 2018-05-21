# HelloWorld1

## Step1: Build and start the server
``` 
stack build
stack exec faeServer
```

## Step2: Execute HelloWorld1
```
cd demos/HelloWorld1/
stack exec POSTTX HelloWorld1
```
The result should look something like this:
``` 
Transaction f6df16bc11a23a66a2be5fadbd2470cf39abfb6de0d08f780223d48f8b3eb4f0:
  result: "Hello, world 1!"
  outputs:
  txSigners:
    self: 800725c2cbf8c17ef4244222ae12308cebd6d111cee08fd5a4300688ae833a52
```
