%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DCG for Parsing .poster Files

:- use_module(io).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main Parsing Predicate

% parse_poster(+Poster, +Elements)
%  Parses a poster file and normalizes its elements into a structured format.
parse_poster(Poster, Elements) :-
    clean_elements(Elements, CleanedElements),
    phrase(poster(Poster), CleanedElements).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Cleaning and Normalizing Elements


% clean_elements(+Elements, -CleanedElements)
% Predicate to clean and normalize list Elements so that it fits the format needed for parsing
% Base case: empty list
clean_elements([], []).

% Case: Skip empty elements
clean_elements([' '|Rest], Cleaned) :-
    clean_elements(Rest, Cleaned).

% Case: Skip newline
clean_elements(['\n'|Rest], Cleaned) :-
    clean_elements(Rest, Cleaned).

% Case: Skip tab
clean_elements(['\t'|Rest], Cleaned) :-
    clean_elements(Rest, Cleaned).

% Case: Skip quotes
clean_elements(['\"'|Rest], Cleaned) :-
    clean_elements(Rest, Cleaned).

% Case: Handle quoted strings
clean_elements([Element|Rest], [Content|CleanedRest]) :-
    atom_concat('"', Inner, Element),
    atom_concat(InnerContent, '"', Inner),
    atom_string(InnerContent, Content),
    clean_elements(Rest, CleanedRest).

% Case: Retain valid elements
clean_elements([Element|Rest], [Element|CleanedRest]) :-
    clean_elements(Rest, CleanedRest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DCG Rules

% poster(-Poster)
% Overall poster format
poster([poster(Dimensions, Filename) | Assets]) -->
    [poster,:],
    dimensions(Dimensions),
    filename(Filename),
    assets(Assets).

% dimensions(-Dimensions)
% Parse dimensions
dimensions(dimensions(Width, Height)) -->
    [dimensions,:,DimensionsStr],
    { split_string(DimensionsStr, "x", "", [WidthStr, HeightStr]),
      atom_number(WidthStr, Width),
      atom_number(HeightStr, Height) }.

% filename(-Filename)
% Parse filename
filename(filename(Filename)) -->
    [filename,:,Filename].

% assets(-Assets)
% Base case: empty list --> No more assets to parse
assets([]) --> [].

% Parse the list of assets
assets([Asset | Rest]) -->
    asset(Asset),
    assets(Rest).

% asset(-Asset)
% Handle asset types
% Case: parse title
asset([title(Properties)]) -->
    [title,:],
    parse_properties(Properties).
% Case: parse section
asset([section(Properties)]) -->
    [section,:],
    parse_properties(Properties).
% Case: parse text
asset([text(Properties)]) -->
    [text,:],
    parse_properties(Properties).
% Case: parse figure
asset([figure(Properties)]) -->
    [figure,:],
    parse_properties(Properties).
% Case: parse caption
asset([caption(Properties)]) -->
    [caption,:],
    parse_properties(Properties).
% Case: parse image
asset([image(Properties)]) -->
    [image,:],
    parse_properties(Properties).

% parse_properties(-Properties)
% Predicate to parse dimensions
parse_dimensions(Dimensions, Width, Height) :-
    split_string(Dimensions, "x", "", [WidthStr, HeightStr]),
    atom_number(WidthStr, Width),
    atom_number(HeightStr, Height).

% Recursive parsing of properties for any asset
parse_properties([Property | Rest]) -->
    parse_property(Property),
    parse_properties(Rest).

% Base case: empty list --> No more properties to parse
parse_properties([]) --> [].

% parse_property(-Property)
% Parsing individual properties
% Case: parse content
parse_property(content(Content)) -->
    [content,:],
    content_value(Content).
% Case: parse position
parse_property(position(Position)) -->
    [position,:,Position].
% Case: parse size
parse_property(size(Width, Height)) -->
    [size,:,Dimensions],
    { parse_dimensions(Dimensions, Width, Height) }.
% Case: parse aspect
parse_property(aspect(Width, Height)) -->
    [aspect,:,Dimensions],
    { parse_dimensions(Dimensions, Width, Height) }.
% Case: parse source
parse_property(source(Source)) -->
    [source,:,Source].
% Case: parse width
parse_property(width(WidthPercentage)) -->
    [width,:,WidthPercentage].
% Case: parse height
parse_property(height(HeightPercentage)) -->
    [height,:,HeightPercentage].
% Case: parse ref
parse_property(ref(Ref)) -->
    [ref,:,Ref].
% Case: parse adjacency
parse_property(adjacency(_Adjacency)) -->
    [adjacency, :, Relation, Ref],
    { atom_string(Relation, _RelationStr),
      atom_string(Ref, _RefStr)}.

% This parses the content value (i.e., what comes after the content property).
% This is done in a very bad manner by reading one element at a time,
% adding it to the list of Words until a word is encountered that is part of the list:
% "position", "size", "title", "figure", "text", "caption", "image", "section", "ref", "adjacency".
%
% This works, but it imposes a constraint on the input a user can provide, since if they use one of these words in their content,
% the parser will fail.
%
% TODO: I need to improve this.
% Since the text after the content property is written between quotation marks,
% I should make it so the words between quotation marks are treated as one whole.

% content_value(-Content)
content_value(Content) -->
    [Word],
    { atom_string(Word, WordStr),
      \+ memberchk(WordStr, ["position", "size", "title", "figure", "text", "caption", "image", "section", "ref", "adjacency"]) },
    content_value(Rest),
    { string_concat(WordStr, " ", Temp), string_concat(Temp, Rest, Content) }.

content_value(Content) -->
    [Word],
    { atom_string(Word, WordStr),
      \+ memberchk(WordStr, ["position", "size", "title", "figure", "text", "caption", "image", "section", "ref", "adjacency"]),
      Content = WordStr }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% User definitions

layout_boxes(Layout, Layout).

% Extract the box ID
box_id(box(id = Id, _, _, _, _, _, _, _), Id).

% Extract the box type
box_type(box(_, type = Type, _, _, _, _, _, _), Type).

% Extract the box row
box_row(box(_, _, row = (R0, R1), _, _, _, _, _), R0, R1).

% Extract the box column
box_col(box(_, _, _, col = (C0, C1), _, _, _, _), C0, C1).

% Extract the box content
box_content(box(_, _, _, _, content = Content, _, _, _), Content).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% extract_filename(+Poster, -Filename)
% Extract filename from the Poster
extract_filename([poster(_, filename(Filename)) | _], Filename).

% extract_dimensions(+Poster, -Width, -Height)
% Extract dimensions from the Poster
extract_dimensions([poster(dimensions(Width, Height), _) | _], Width, Height).

% extract_assets(+Poster, -Assets)
% Extract the list of Assets from the Poster
extract_assets([poster(_, _) | Assets], Assets).

% transform_to_boxes(+Assets, -Boxes, +Width, +Height)
% Predicate to transform assets into boxes
transform_to_boxes(Assets, Boxes, Width, Height) :-
    transform_assets_to_boxes(Assets, 1, [], Boxes, Width, Height).
% 1 Serves as the ID for the first box, this will be incremented after each box is created thus giving them all a different ID

% transform_assets_to_boxes(+Assets, +Counter, +BoxesAcc, -Boxes, +Width, +Height)
% Base case: no more assets to process
transform_assets_to_boxes([], _, BoxesAcc, BoxesAcc, _Width, _Height).

% Recursive case: handle 'title' or 'section' assets (type = header)
transform_assets_to_boxes([Asset | Rest], Counter, BoxesAcc, Boxes, Width, Height) :-
    Asset = [Temp], % Extract single Asset
    handle_asset(Temp, Counter, BoxesAcc, NewCounter, NewBoxesAcc, Width, Height),
    transform_assets_to_boxes(Rest, NewCounter, NewBoxesAcc, Boxes, Width, Height).

% handle_asset(+Asset, +Counter, +BoxesAcc, -NewCounter, -NewBoxesAcc, +Width, +Height)
% Handle the Assets
handle_asset(title(Properties), Counter, BoxesAcc, NewCounter, NewBoxesAcc, Width, Height) :-
    Type = header, % title becomes header
    handle_text_asset(Type, Properties, Counter, NewCounter, BoxesAcc, NewBoxesAcc, Width, Height).

handle_asset(section(Properties), Counter, BoxesAcc, NewCounter, NewBoxesAcc, Width, Height) :-
    Type = header,  % section becomes header
    handle_text_asset(Type, Properties, Counter, NewCounter, BoxesAcc, NewBoxesAcc, Width, Height).

handle_asset(text(Properties), Counter, BoxesAcc, NewCounter, NewBoxesAcc, Width, Height) :-
    Type = text,
    handle_text_asset(Type, Properties, Counter, NewCounter, BoxesAcc, NewBoxesAcc, Width, Height).

handle_asset(figure(Properties), Counter, BoxesAcc, NewCounter, NewBoxesAcc, Width, Height) :-
    Type = image,  % figure becomes image
    handle_image_asset(Type, Properties, Counter, NewCounter, BoxesAcc, NewBoxesAcc, Width, Height).

handle_asset(caption(Properties), Counter, BoxesAcc, NewCounter, NewBoxesAcc, Width, Height) :-
    Type = text, % caption becomes text
    handle_text_asset(Type, Properties, Counter, NewCounter, BoxesAcc, NewBoxesAcc, Width, Height).

handle_asset(image(Properties), Counter, BoxesAcc, NewCounter, NewBoxesAcc, Width, Height) :-
    Type = image,
    handle_image_asset(Type, Properties, Counter, NewCounter, BoxesAcc, NewBoxesAcc, Width, Height).

% handle_image_asset(+Type, +Properties, +Counter, -NewCounter, +BoxesAcc, -NewBoxesAcc, +Width, +Height)
% Transform image asset into a box
handle_image_asset(Type, Properties, Counter, NewCounter, BoxesAcc, [Box | BoxesAcc], Width, Height) :-
    % Extract properties
    extract_property(Properties, position(Position), position('none')), % if there is no position use default: position('none')
    extract_property(Properties, source(Source), source('notfound.jpg')), % if there is no source use default: source(notfound.jpg)
    extract_property(Properties, width(WPercentage), width('99%')), % not 100% because 100% has issues with contraints
    extract_property(Properties, height(HPercentage), height('99%')),
    extract_percentage(WPercentage, WidthPercentage), % This will remove the % symbol
    extract_percentage(HPercentage, HeightPercentage),
    % Construct the box
    construct_box(Counter, Type, Position, Source, Source, Box, Width, Height, WidthPercentage, HeightPercentage),
    NewCounter is Counter + 1. % Increment counter for the ID of the next Box

% handle_text_asset(+Type, +Properties, +Counter, -NewCounter, +BoxesAcc, -NewBoxesAcc, +Width, +Height)
% Transform text asset into a box
handle_text_asset(Type, Properties, Counter, NewCounter, BoxesAcc, [Box | BoxesAcc], Width, Height) :-
    % Extract properties
    extract_property(Properties, content(Content), content([])),
    extract_property(Properties, position(Position), position('none')),
    extract_property(Properties, width(WPercentage), width('99%')),
    extract_property(Properties, height(HPercentage), height('99%')),
    extract_percentage(WPercentage, WidthPercentage),
    extract_percentage(HPercentage, HeightPercentage),
    % Construct the box
    construct_box(Counter, Type, Position, Content, empty, Box, Width, Height, WidthPercentage, HeightPercentage),
    NewCounter is Counter + 1.  % Increment counter for the ID of the next Box

% extract_percentage(+PercentageAtom, -Number)
% Removes the % symbol of a percentage so that the raw number can be extracted
extract_percentage(PercentageAtom, Number) :-
    atom_chars(PercentageAtom, Chars),        % Convert the atom to a list of characters
    append(NumberChars, ['%'], Chars),       % Separate the number characters from the '%'
    atom_chars(NumberAtom, NumberChars),     % Convert the number characters back to an atom
    atom_number(NumberAtom, Number).         % Convert the atom to a number this number is the output


% extract_property(+Properties, -Property, +Default)
% Extracts a specific property from the properties list and returns the default if not found
% base case: property not found, return Default
extract_property([], Default, Default).
% Case: property found, return it
extract_property([Property | _], Property, _Default).
% Case: property not found try next
extract_property([_ | Rest], Property, Default) :-
    extract_property(Rest, Property, Default).


% construct_box(+Id, +Type, +Position, +Content, +Source, -Box, +Width, +Height, +WidthPercentage, +HeightPercentage)
% Predicate to construct a box from components
construct_box(Id, Type, Position, Content, Source,  box(id = Id,
                                               type = Type,
                                               row = (RowStart, RowEnd),
                                               col = (ColStart, ColEnd),
                                               content = Content,
                                               ref =  Source,
                                               widthpercentage = WidthPercentage,
                                               heightpercentage = HeightPercentage), Width, Height, WidthPercentage, HeightPercentage) :-
    apply_constraints(Position, RowStart, RowEnd, ColStart, ColEnd, Width, Height, WidthPercentage, HeightPercentage).
% Apply constraints on (RowStart, RowEnd), (ColStart, ColEnd)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Constraints
% Had a lot of issues with the constraints, I would need to think logically again and take my time with it to have a better result.
% I also had a bad image of how to contruct the poster based on the Position (top-edge, bottom, edge, left-edge, ect..)
% So if i had more time i would do it over and think more about how to have a good logical structure for the poster
:- use_module(library(clpfd)).

% apply_constraints(+Position, -RowStart, -RowEnd, -ColStart, -ColEnd, +Width, +Height, +WidthPercentage, +HeightPercentage)
% Apply constraints based on the position
apply_constraints('top-edge', RowStart, RowEnd, ColStart, ColEnd, Width, Height, WidthPercentage, HeightPercentage) :-
    RowStart #< RowEnd,
    ColStart #< ColEnd,
    RowStart #= 1,
    RangeHeight #= (Height // 4) - RowStart,
    RowEnd #>= RowStart + (RangeHeight * HeightPercentage // 100),
    RowEnd #=< Height // 4,
    ColStart #>= Width // 4,
    RangeWidth #= (Width - (Width // 4)) - ColStart,
    ColEnd #>= ColStart + (RangeWidth * WidthPercentage // 100),
    ColEnd #=< Width - (Width // 4).

apply_constraints('bottom-edge', RowStart, RowEnd, ColStart, ColEnd, Width, Height, WidthPercentage, HeightPercentage) :-
    RowStart #< RowEnd,
    ColStart #< ColEnd,
    RowStart #= Height - (Height // 4),
    RangeHeight #= (Height - RowStart),
    RowEnd #>= RowStart + (RangeHeight * HeightPercentage // 100),
    RowEnd #=< Height + 1,
    ColStart #>= Width // 4,
    RangeWidth #= (Width - (Width // 4)) - ColStart,
    ColEnd #>= ColStart + (RangeWidth * WidthPercentage // 100),
    ColEnd #=< Width - (Width // 4).

apply_constraints('left-edge', RowStart, RowEnd, ColStart, ColEnd, Width, Height, WidthPercentage, HeightPercentage) :-
    RowStart #< RowEnd,
    ColStart #< ColEnd,
    RowStart #>= Height // 4,
    RowEnd #=< Height - (Height // 4),
    SizeHeight #= RowEnd - RowStart,
    SizeHeight #= Height // 4 * HeightPercentage // 100, % This makes it so you can have 2 Assets on the edge
    ColStart #= 1,
    RangeWidth #= Width // 4,
    ColEnd #= ColStart + (RangeWidth * WidthPercentage // 100),
    ColEnd #=< Width // 4.

apply_constraints('right-edge', RowStart, RowEnd, ColStart, ColEnd, Width, Height, WidthPercentage, HeightPercentage) :-
    RowStart #< RowEnd,
    ColStart #< ColEnd,
    RowStart #>= Height // 4,
    RowEnd #=< Height - (Height // 4),
    SizeHeight #= RowEnd - RowStart,
    SizeHeight #= Height // 4 * HeightPercentage // 100,
    ColStart #= Width - (Width // 4),
    RangeWidth #= Width // 4,
    ColEnd #= ColStart + (RangeWidth * WidthPercentage // 100),
    ColEnd #=< Width.

apply_constraints('top-left', RowStart, RowEnd, ColStart, ColEnd, Width, Height, WidthPercentage, HeightPercentage) :-
    RowStart #< RowEnd,
    ColStart #< ColEnd,
    RowStart #= 1,
    RangeHeight #= (Height // 4) - RowStart,
    RowEnd #= RowStart + (RangeHeight * HeightPercentage // 100),
    RowEnd #=< Height // 4,
    ColStart #= 1,
    RangeWidth #= (Width // 4) - ColStart,
    ColEnd #= ColStart + (RangeWidth * WidthPercentage // 100),
    ColEnd #=< Width // 4.

apply_constraints('bottom-right', RowStart, RowEnd, ColStart, ColEnd, Width, Height, WidthPercentage, HeightPercentage) :-
    RowStart #< RowEnd,
    ColStart #< ColEnd,
    RowEnd #= Height + 1,
    RangeHeight #= RowEnd - (Height - (Height // 4)),
    RowStart #= RowEnd - (RangeHeight * HeightPercentage // 100),
    RowStart #>= Height - (Height // 4),
    ColEnd #= Width + 1,
    RangeWidth #= ColEnd - (Width - (Width // 4)),
    ColStart #= ColEnd - (RangeWidth * WidthPercentage // 100),
    ColStart #>= Width - (Width // 4).

apply_constraints(_, RowStart, RowEnd, ColStart, ColEnd, Width, Height, _WidthPercentage, _HeightPercentage) :-
    % Put the rest in the center
    RowStart #< RowEnd,
    ColStart #< ColEnd,
    RowStart #>= Height // 4,
    RowStart #< Height - (Height // 4),
    RowEnd #=< Height - (Height // 4),
    SizeHeight #= RowEnd - RowStart,
    SizeHeight #= Height // 8, % This makes it so you can have 4 Assets in the center
    ColStart #>= (Width // 4),
    ColStart #< Width - (Width // 4),
    RangeWidth #= (Width - (Width // 4)) - ColStart,
    ColEnd #= ColStart + (RangeWidth * 100 // 100),
    ColEnd #> ColStart, ColEnd #=< Width - (Width // 4).

% no_overlap(+Boxes)
% Enforce no-overlap constraints for all pairs of boxes
no_overlap([]). % Base case: No boxes left to compare
no_overlap([Box | Boxes]) :-
    maplist(non_overlapping(Box), Boxes), % Ensure the current box does not overlap with any other
    no_overlap(Boxes). % Do the same for the remaining boxes


% non_overlapping(+Box1, +Box2)
% Enforce non-overlapping constraints between two boxes
non_overlapping(box(id=_, _, row=(RowStartA, RowEndA), col=(ColStartA, ColEndA), _, _, _, _),
                box(id=_, _, row=(RowStartB, RowEndB), col=(ColStartB, ColEndB), _, _, _, _)) :-
    RowEndA #=< RowStartB ;
    RowStartA #>= RowEndB ;
    ColEndA #=< ColStartB ;
    ColStartA #>= ColEndB.  % If one of these is true then the boxes are not overlapping


% apply_constraints_to_box(+Box)
apply_constraints_to_box(box(id=_ID, type=_Type, row=(RowStart, RowEnd), col=(ColStart, ColEnd), content=_Content, ref=_Ref, widthpercentage = _WidthPercentage, heightpercentage= _HeightPercentage)) :-
    labeling([ffc], [RowStart, RowEnd, ColStart, ColEnd]).

% generate_html(+NameOfPoster)
generate_html(NameOfPoster) :-
    io:input(NameOfPoster, Elements),          % Read .poster file into Elements
    parse_poster(Poster, Elements),            % Parse the Elements into Poster
    extract_filename(Poster, Name),
    extract_dimensions(Poster, Width, Height),
    extract_assets(Poster, Assets),
    transform_to_boxes(Assets, Boxes, Height, Width), % Create the boxes
    no_overlap(Boxes), % Apply the no overlapping contraint
    maplist(apply_constraints_to_box, Boxes), % use labeling to give real values to the contrained variables
    output(Boxes, Name, Height, Width). % Generate the HTML output

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Testing

all_tests :- % If the test succeeds
    writeln('Testing: Parsing filename'),
    test_parse_filename,
    writeln('Testing: Parsing dimensions'),
    test_parse_dimensions,
    writeln('Testing: Parsing'),
    test_parse_poster,
    writeln('Testing: Extracting elements'),
    test_extract_poster_elements.

all_tests :- % If the test fails
    writeln('Some tests failed.').

test_parse_filename :- % If the test succeeds
    Atoms = ['filename',':','volcano.html'],
    phrase(filename(Filename), Atoms),
    writeln('Parsing test passed:'),
    writeln(Filename).

test_parse_filename :- % If the test fails
    writeln('Parsing filename failed.').

test_parse_dimensions :- % If the test succeeds
    Atoms = ['dimensions',':','1920x1080'],
    phrase(dimensions(dimensions(Width, Height)), Atoms),
    writeln('Parsing test passed:'),
    writeln('Parsed dimensions:'),
    writeln('Width: '), writeln(Width),
    writeln('Height: '), writeln(Height).

test_parse_dimensions :-% If the test fails
    writeln('Parsing dimensions failed.').

test_parse_poster :- % If the test succeeds
    Atoms = ['poster',':','\n','\t','dimensions',':','1920x1080','\n','\t','filename',':','"','volcano.html','"'],
    parse_poster(ParsedPoster, Atoms),
    writeln('Parsing test passed:'),
    writeln(ParsedPoster).

test_parse_poster :-% If the test fails
    writeln('Parsing test failed.').

test_extract_poster_elements :- % If the test succeeds
    ParsedPoster = [poster(dimensions(25,35),filename('volcano.html')),
    [title([position('top-edge'),width('100%'),content('Volcanoes')])],
    [section([position('left-edge'),content('Definition'),ref('definition')])]
    ],
    extract_filename(ParsedPoster, Filename),
    extract_dimensions(ParsedPoster, Width, Height),
    extract_assets(ParsedPoster, Assets),
    writeln('Extraction test passed:'),
    write('Filename: '), writeln(Filename),
    write('Width: '), writeln(Width),
    write('Height: '), writeln(Height),
    write('Assets: '), writeln(Assets).

test_extract_poster_elements :- % If the test fails
    writeln('Extraction test failed.').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

