# HelloWorld2

## Step1: Build and start the server
``` 
stack build
stack exec faeServer
```

## Step2: Execute HelloWorld2
```
cd demos/HelloWorld2/
stack exec POSTTX HelloWorld2
```

The result should look something like this:
``` 
Transaction 1685a9d0c34639f47b152849b0b5984f8f3b37ee1203635154955869bdc8883d:
  result: ()
  outputs:
    0: 00b628bdff52ad413d19b094646ee0685e22931037fbca595dd4e066f10bf431
  txSigners:
    self: 800725c2cbf8c17ef4244222ae12308cebd6d111cee08fd5a4300688ae833a52
```

Now execute CallHelloWorld2 to print it's contents:
``` 
stack exec txID=1685a9d0c34639f47b152849b0b5984f8f3b37ee1203635154955869bdc8883d CallHelloWorld2
```
