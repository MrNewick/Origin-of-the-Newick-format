# Origin-of-the-Newick-format
The first release in 1984 of the format for encoding phylogenetic trees that was adopted as the Newick format in 1986.

I worked with Joe Felsenstein in his lab in the Genetics Department at the University of Washington from about the beginning of April through the end of August in 1984.  Joe had no graphics capability in his phylogeney inference package PHYLIP.  I wrote a program 'PLOTREE' to draw trees on any graphics device of the time.  We needed a way of transfering a tree calculated by one of his programs to my plotting program.  I developed a nested-parenthesis format to fill this need.  The program was included in Joe's next release of PHYLIP (version 2.61) in December, 1984.  A .tar file of PHYLIP version 2.61 is in this repository along with the documentation and Pascal code of PLOTREE.  This and other old versions of PHYLIP can be found at Joe's page http://evolution.genetics.washington.edu/phylip/oldversions.html.  PLOTREE was written in Turbo Pascal on an Osborne-1.  (Joe's brother, Lee, designed the Osborne, the earliest portable computer.)  Turbo Pascal version 2.0 was released on April 17, 1984, so initial development was probably in Turbo Pascal 1.0.  The complier interface looked like this on the Osborne CRT screen: http://progopedia.com/version/turbo-pascal-2.0/

The documentation of PLOTREE includes this discription of the format for encoding phylogenetic trees:

INPUT FORMAT OF TREE DESCRIPTIONS

         Plotree accepts tree descriptions as input from the file 
    "treefile."  Plotree requires a nested parentheses notation to 
    describe the tree to be drawn.  The unit in this notation might 
    be termed a "node description" because it describes the tree at a 
    single node.  A node description consists of (1) the label of the 
    node and (2) if there exists any immediately descendant node or 
    nodes, a list of the node descriptions for the immediately 
    descendant nodes.  This list of node descriptions is enclosed by 
    parentheses and the descendant node descriptions are separated by 
    commas.  It is easy to see that this notation results in a nested 
    series of parentheses.  The tree description is terminated by a 
    '*'.  Here are some trees and their notation:

                                             C   D   F   G
                                              \ /     \ / 
                                       C       B       E
           B    B     C   B  C  D      !        \     /
           !     \   /     \ ! /       B         \   /
           !      \ /       \!/        !          \ /
           A       A         A         A           A

    Tree   1       2         3         4           5


    Notation for the above trees:

                          1.  A(B)*
                          2.  A(B,C)*
                          3.  A(B,C,D)*
                          4.  A(B(C))*
                          5.  A(B(C,D),E(F,G))*

    None of the nodes are required to be labeled.  For example, if 
    all of the internal nodes (that is, nodes A, B, and E) on tree 5 
    above were unlabeled, then the notation would be

                             ((C,D),(F,G))*

    If all labels were removed from tree 5, the notation would be

                               ((,),(,))*

    Labels may consist of any sequence of upper- or lower-case 
    alphabetic or numeric characters.  They may not contain blanks 
    (spaces).  Punctuation marks and other special characters will 
    not be plotted.  The present version of Plotree will truncate 
    labels that are longer that three characters.  (This maximum 
    length of labels is controlled by a constant at the beginning of 
    the program.)  More than one label may be associated with a 
    single node, however the current version of Plotree will only 
    plot the first label.  Multiple labels are separated by blanks.
         Lengths of internodes may also be specified.  The length of 
    an internode is associated with the node above that internode.  
    The length follows the node label and is separated from the node 
    label by a colon (":").  For example, the tree

                                        C
                               B       /   
                                \     /  
                             1.5 \   / 2.0
                                  \ /    
                                   A

    would have the notation

                       A(B:1.5,C:2.0)*

    Lengths may have decimal points or not.  Blanks may appear 
    anywhere within the tree notation except within labels or within 
    lengths (e.g. '1. 52' is not a valid length).
         Any number of tree descriptions may be placed in treefile 
    and may continue to as many lines as necessary, but each tree 
    description must begin on a new line.
