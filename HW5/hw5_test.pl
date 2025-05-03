:- use_module(library(plunit)).
:- use_module(hw5).

:- begin_tests(hw5_test).

% Test instance creation - fixed to not leave choicepoints
test(new_instance_concrete) :-
    new_instance(i, stage),
    assertion(instance(i, stage)),  % Verify instance was created
    !.  % Cut to prevent choicepoints

test(new_instance_abstract, [fail]) :-
    new_instance(j, lane).

% Test adding children and retrieving children/descendants
test(add_child_swimlane_to_stage) :-
    new_instance(s1, stage),
    new_instance(l3, swimlane),
    add_child(l3, s1),
    children(s1, C),
    assertion(C == [l3]),
    !.  % Cut to prevent choicepoints

test(descendants_simple) :-
    new_instance(s1, stage),
    new_instance(l3, swimlane),
    add_child(l3, s1),
    descendants(s1, D),
    assertion(D == [l3]),
    !.  % Add cut to prevent choicepoints

test(cannot_add_different_class_child, [fail]) :-
    new_instance(s1, stage),
    new_instance(s2, stage),
    new_instance(l3, swimlane),
    add_child(l3, s1),
    add_child(s2, s1).

test(add_multiple_children) :-
    new_instance(s1, stage),
    new_instance(l3, swimlane),
    new_instance(l4, swimlane),
    new_instance(s2, stage),
    add_child(l3, s1),
    add_child(s2, l3),
    add_child(l4, s1),
    children(s1, C),
    assertion(C == [l3, l4]),
    !.  % Add cut to prevent choicepoints

test(descendants_complex) :-
    new_instance(s1, stage),
    new_instance(s2, stage),
    new_instance(l3, swimlane),
    add_child(l3, s1),
    add_child(s2, l3),
    add_child(l4, s1),
    descendants(s1, D),
    assertion(D == [l3, s2, l4]),
    !.  % Add cut to prevent choicepoints

% Test ancestry restrictions
test(cannot_add_ancestor_as_child, [fail]) :-
    new_instance(s1, stage),
    new_instance(l3, swimlane),
    new_instance(s2, stage),
    add_child(l3, s1),
    add_child(s2, l3),
    add_child(s1, s2),
    !.  % Add cut to prevent choicepoints

test(cannot_add_descendant_as_child, [fail]) :-
    new_instance(s1, stage),
    new_instance(l3, swimlane),
    new_instance(s2, stage),
    add_child(l3, s1),
    add_child(s2, l3),
    add_child(s2, s1),
    !.  % Add cut to prevent choicepoints

% Test multiple parent restriction
test(no_multiple_parents, [fail]) :-
    new_instance(l4, swimlane),
    new_instance(l5, swimlane),
    new_instance(s2, stage),
    add_child(l5, l4),
    add_child(l5, s2),
    !.  % Add cut to prevent choicepoints

% Test valid_children
test(valid_children_s1) :-
    new_instance(s1, stage),
    new_instance(l3, swimlane),
    new_instance(l4, swimlane),
    add_child(l3, s1),
    add_child(l4, s1),
    assertion(valid_children(s1)),
    !.  % Add cut to prevent choicepoints

test(valid_children_l3) :-
    new_instance(l3, swimlane),
    new_instance(s2, stage),
    add_child(s2, l3),
    assertion(valid_children(l3)),
    !.  % Add cut to prevent choicepoints

:- end_tests(hw5_test).

