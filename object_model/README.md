# Object model

Def object model, that implements:

- class
- attribute 
- access control
- multiple inheritance
- dynamic polymorphism
- method combination
- command-query separation

## Usage

### Class

Class have state which described by slots
Class can inherit multiple classes
Class declared by defclass
Classes are keywords

### Slots

Slots can be public and private
Slots can have default value default is nil
Public slot can be accessed anywhere
Private slot can be accessed in methods of that class
Inherited public slots still public in child class
In case of romb inheritance you need to specify path to destination, default first in declaration
Names of slots are keywords

### Objects

instanceate with make-instance, provide initvalues for slots also for parents or default value will be assigned
Access to private out of methods slots will throw nullpointer
You can access private slots in methods

### Methods
dispatching:
    first arguments are more valueble
    if class or parent matches to specializer:
        class of object -> parent classes of object (sorts in order of multi inheretance declaration) -> parent parent classes of object -> ...
    else applies generic
order:
    1. before in order from most specific
    2. main function
    3. after in order from least specific
values of before and after methods ignored, they used for their side effects
methods are keywords and dispatches with dispatch
if :query objects are copied and you can't change them
if :command objects are not copied and you can change them

### API
- defclass [classname=symbol (list classname=registred-classnames) (list slot)]
- slot [slotname=symbol access-type={:public :private} default-value=any-default-nil]
- make-instance [classname=registred-classname hash-init [hash-class-init]]
- get-value [object=instance-of-class slot-name=symbol [classname=registred-parent-class]]
- set-value [object=instance-of-class slot-name=symbol value [classname=registred-parent-class]]
- register-method [method-name=symbol command-query={:command :query} specifier={:before :main :after} [vector type={'bool-type 'number-type 'str-type 'symbol-type 'keyword-type 'collection 'generic-type classname=registred-class}] function]
- dispatch-method [methodname=registred-class [vector args]]
- call-next-method [methodname=registred-class [vector args]]
- call-next-method [context=returned-after-call-next-method]
- call-next-method-result [context=returned-after-call-next-method]


