:- module(hw5, [
    new_instance/2,
    instance/2,
    add_child/2,
    child/2,
    parent/2,
    children/2,
    descendants/2,
    valid_children/1
]).

% Define class hierarchy
class(stage).
class(lane).  % Abstract class
class(swimlane).

% Define subclass relationships
subclass(swimlane, lane).
subclass(stage, lane).

% Define abstract classes
abstract(lane).

% Define valid parent-child relationships
valid_child_type(stage, swimlane).
valid_child_type(swimlane, stage).

% Dynamic predicates to track instances and relationships
:- dynamic instance_of/2.
:- dynamic child_of/2.

% Create a new instance of a class
new_instance(Instance, Class) :-
    class(Class),
    \+ abstract(Class),
    assertz(instance_of(Instance, Class)).

% Check if an instance is of a specific class (direct or inherited)
instance(Instance, Class) :-
    instance_of(Instance, Class).
instance(Instance, Class) :-
    instance_of(Instance, SubClass),
    subclass(SubClass, Class).

% Check if Descendant is a descendant of Ancestor
is_descendant(Ancestor, Descendant) :-
    child_of(Ancestor, Intermediate),
    (Descendant = Intermediate ; is_descendant(Intermediate, Descendant)).

% Add a child to a parent if it satisfies all constraints
add_child(Child, Parent) :-
    % Both must be valid instances
    instance_of(Child, ChildClass),
    instance_of(Parent, ParentClass),
    
    % Check type compatibility
    valid_child_type(ParentClass, ChildClass),
    
    % Child cannot be an ancestor of Parent (would create a cycle)
    \+ is_descendant(Child, Parent),
    
    % Child cannot be the same as Parent (would create a cycle)
    Child \= Parent,
    
    % Remove existing parent relationship if any
    (child_of(OldParent, Child) -> 
        retract(child_of(OldParent, Child))
    ; true),
    
    % Add the relationship
    assertz(child_of(Parent, Child)).

% Check if Child is a child of Parent
child(Child, Parent) :-
    child_of(Parent, Child).

% Check if Parent is a parent of Child
parent(Parent, Child) :-
    child_of(Parent, Child).

% Get all children of an instance
children(Parent, Children) :-
    findall(Child, child_of(Parent, Child), Children).

% Get all descendants of an instance
descendants(Instance, Descendants) :-
    findall(D, is_descendant(Instance, D), Descendants), !.

% Check if an instance has valid children
valid_children(Instance) :-
    instance_of(Instance, InstanceClass),
    \+ (
        child_of(Instance, Child),
        instance_of(Child, ChildClass),
        \+ valid_child_type(InstanceClass, ChildClass)
    ), !.
