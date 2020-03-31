PROGRAM Plotree(parmfile, treefile, fontfile, plotfile, OUTPUT);
   (* PHYLIP version 2.6
      Version 1.0, Sep 1984.  Written by Chris Meacham, Botany Dept.,
      Univ. of Georgia, Athens, Georgia  30602, U.S.A. *)

   CONST maxnodes = 100;
      maxlablen = 3;
      fontsize = 2870; (* for font1: 1013; font2: 1850; font3: 2870 *)
      doswitch = TRUE;
      dontswitch = FALSE;

   TYPE fontname = (font1, font2, font3);
      modetype = (yes, no);
      posoptiontype = (atroot, right, left, above, below, center);
      penstatustype = (penup, pendown);
      plottertype = (radioshack, calcomp, tektronix, hp7470, other);
      fonttype = ARRAY [1..fontsize] OF INTEGER;
      plotstring = ARRAY [1..maxlablen] OF CHAR;
      nodeptr = ^treenode;
      treenode = RECORD
         ancestor, sibling, descendant, lastinlist, 
            left, right : nodeptr;
         c, nbranches, ndlablen : INTEGER;
         nodesize, len, spread, r, theta, x, y, 
         height, width : REAL;
         ndlabel : plotstring
         END;
   VAR nnodes, xnow, ynow, oldxhigh, oldxlow, oldyhigh, 
         oldylow, longestlablen : INTEGER;
      pi, smallnode, radius, diameter, spanunit, 
         padding, spaceval, scrunchval, heightval, 
         scrunchlimit, rotation, xposition, yposition, 
         xunitspercm, yunitspercm, aspectratio, 
         xsize, ysize, xrange, yrange, maxnodesize, labelheight,
         labelsize : REAL;
      xcircle, ycircle : ARRAY [1..30] OF REAL;
      cchex : ARRAY [0..15] OF CHAR;
      parmfile, treefile, fontfile, plotfile : TEXT;
      labelspresent, fontloaded, candraw, parmsok : BOOLEAN;
      fonttoload : fontname;
      font : fonttype;
      sizemode, lengthmode, monitor, penchange : modetype;
      plotter : plottertype;
      xoption, yoption : posoptiontype;
      root, garbage : nodeptr;
      nodelist : ARRAY [1..maxnodes] OF nodeptr;

   PROCEDURE initparms;
      CONST nabs = 21;
      VAR ab1, ab2 : ARRAY [0..nabs] OF CHAR;

      PROCEDURE readparms;
         VAR parmnum : INTEGER;
            ch, c1, c2 : CHAR;

         PROCEDURE getp(VAR ch : CHAR);
         BEGIN (* getp *)
            ch := ' ';
            WHILE (ch=' ') AND NOT EOLN(parmfile) DO
               READ(parmfile, ch);
            END; (* getp *)
   
         FUNCTION setmode : modetype;
         BEGIN (* setmode *)
            getp(ch);
            CASE ch OF
               'Y': setmode := yes;
               'N': setmode := no
               END;
            END; (* setmode *)

         PROCEDURE plotrparms(ch : CHAR);
         BEGIN (* plotrparms *)
            CASE ch OF
               'R': plotter := radioshack;
               'C': BEGIN
                  plotter := calcomp;
                  xunitspercm := 39.37;
                  yunitspercm := 39.37;
                  xsize := 25.0;
                  ysize := 25.0;
                  xposition := 12.5;
                  END;
               'T': BEGIN
                  plotter := tektronix;
                  xunitspercm := 50.0;
                  yunitspercm := 50.0;
                  xsize := 20.46;
                  ysize := 15.6;
                  xposition := 10.23;
                  END;
               'H': BEGIN
                  plotter := hp7470;
                  penchange := yes;
                  xunitspercm := 400.0;
                  yunitspercm := 400.0;
                  xsize := 24.0;
                  ysize := 18.0;
                  xposition := 12.85;
                  yposition := 9.55;
                  yoption := center;
                  rotation := 90.0;
                  END;
               'O': BEGIN
                  plotter := other;
                  (* initial parameter settings for a new 
                     plotter go here *)
                  END
               END;
            END; (* plotrparms *)

      BEGIN (* readparms *)
         WHILE NOT EOF(parmfile) DO BEGIN
            c1 := ' ';
            c2 := ' ';
            WHILE NOT EOLN(parmfile) AND (c1=' ') DO
               READ(parmfile, c1);
            IF NOT EOLN(parmfile) THEN READ(parmfile, c2);
            REPEAT READ(parmfile, ch) UNTIL EOLN(parmfile) OR (ch=' ');
            parmnum := 0;
            WHILE NOT((c1=ab1[parmnum]) AND (c2=ab2[parmnum])) AND
               (parmnum<=nabs) DO parmnum := parmnum+1;
            IF parmnum <= nabs
               THEN CASE parmnum OF
                  1: BEGIN
                     getp(ch);
                     plotrparms(ch);
                     END;
                  2: lengthmode := setmode;
                  3: sizemode := setmode;
                  4: READ(parmfile, diameter);
                  5: READ(parmfile, spanunit);
                  6: READ(parmfile, smallnode);
                  7: READ(parmfile, rotation);
                  8: BEGIN
                     getp(ch);
                     CASE ch OF
                       '1': fonttoload := font1;
                       '2': fonttoload := font2;
                       '3': fonttoload := font3
                        END;
                     END;
                  9: READ(parmfile, xunitspercm);
                  10: READ(parmfile, yunitspercm);
                  11: READ(parmfile, xsize);
                  12: READ(parmfile, ysize);
                  13: READ(parmfile, xposition);
                  14: READ(parmfile, yposition);
                  15: BEGIN
                     getp(ch);
                     CASE ch OF
                        'O': xoption := atroot;
                        'C': xoption := center;
                        'R': xoption := right;
                        'L': xoption := left
                        END;
                     END;
                  16: BEGIN
                     getp(ch);
                     CASE ch OF
                        'O': xoption := atroot;
                        'C': xoption := center;
                        'A': xoption := above;
                        'B': xoption := below
                        END;
                     END;
                  17: READ(parmfile, padding);
                  18: READ(parmfile, maxnodesize);
                  19: monitor := setmode;
                  20: penchange := setmode;
                  21: READ(parmfile, labelsize)
                  END
               ELSE BEGIN
                  WRITELN(OUTPUT, ' PARAMETER LABEL THAT BEGINS ''',
                     c1, c2, ''' IS NOT A LEGITIMATE LABEL');
                  WRITELN(OUTPUT, '  REMEMBER THAT PARAMETER LABELS ',
                     'ARE ENTIRELY UPPER-CASE LETTERS');
                  parmsok := FALSE;
                  READLN(parmfile);
                  END;
            READLN(parmfile);
            END;
         END; (* readparms *)

   BEGIN (* initparms *)
      ab1[0] := ' '; ab2[0] := ' ';
      ab1[1] := 'P'; ab2[1] := 'L'; ab1[2] := 'L'; ab2[2] := 'E';
      ab1[3] := 'S'; ab2[3] := 'I'; ab1[4] := 'N'; ab2[4] := 'O';
      ab1[5] := 'I'; ab2[5] := 'N'; ab1[6] := 'S'; ab2[6] := 'M';
      ab1[7] := 'R'; ab2[7] := 'O'; ab1[8] := 'F'; ab2[8] := 'O';
      ab1[9] := 'X'; ab2[9] := 'U'; ab1[10] := 'Y'; ab2[10] := 'U';
      ab1[11] := 'X'; ab2[11] := 'S'; ab1[12] := 'Y'; ab2[12] := 'S';
      ab1[13] := 'X'; ab2[13] := 'P'; ab1[14] := 'Y'; ab2[14] := 'P';
      ab1[15] := 'X'; ab2[15] := 'O'; ab1[16] := 'Y'; ab2[16] := 'O';
      ab1[17] := 'P'; ab2[17] := 'A'; ab1[18] := 'M'; ab2[18] := 'A';
      ab1[19] := 'M'; ab2[19] := 'O'; ab1[20] := 'P'; ab2[20] := 'E';
      ab1[21] := 'L'; ab2[21] := 'A';
      garbage := NIL;
      pi := 3.1415926536;
      heightval := 0.9;
      scrunchval := 0.8;
      scrunchlimit := 0.01;
      padding := 1.1;
      parmsok := TRUE;
      labelspresent := FALSE;
      fontloaded := FALSE;
      monitor := no;
      penchange := no;
      lengthmode := no;
      sizemode := yes;
      fonttoload := font1;
      plotter := radioshack;
      labelsize := 1.0;
      xunitspercm := 50.0;
      yunitspercm := 50.0;
      xsize := 9.6;
      ysize := 15.0;
      xposition := 4.8;
      yposition := 0.0;
      xoption := center;
      yoption := above;
      rotation := 0.0;
      maxnodesize := 4.0;
      smallnode := 0.6;
      spanunit := 3.0;
      diameter := 1.0;
      readparms;
      padding := padding*diameter;
      IF (SQR(spanunit)-SQR(padding/2.0)) < 0.0
         THEN BEGIN
            WRITELN(OUTPUT, ' INTERNODE LENGTH IS TOO SMALL IN ',
               'RELATION TO NODE DIAMETER');
            parmsok := FALSE;
            END
         ELSE spaceval := 2.0*ARCTAN((padding/2.0)/SQRT(SQR(spanunit)-
            SQR(padding/2.0)));
      aspectratio := yunitspercm/xunitspercm;
      labelsize := labelsize*0.9;
      END; (* initparms *)

   PROCEDURE initnodes(p, bottom : nodeptr);
   BEGIN (* initnodes *)
      IF p <> NIL
      THEN BEGIN
         initnodes(p^.descendant, bottom);
         IF p <> bottom THEN initnodes(p^.sibling, bottom);
         p^.left := p;
         p^.right := p;
         p^.height := 0.0;
         p^.width := 0.0;
         p^.r := 0.0;
         p^.theta := 0.0;
         END;
      END; (* initnodes *)

   PROCEDURE readtree;
      VAR i, count : INTEGER;
         ch : CHAR;
         node : nodeptr;

      PROCEDURE newnode(VAR p : nodeptr);
      BEGIN (* newnode *)
         IF garbage <> NIL
            THEN BEGIN
               p := garbage;
               garbage := garbage^.descendant;
               END
            ELSE NEW(p);
         count := count+1;
         nodelist[count] := p;
         p^.c := count;
         p^.ancestor := NIL;
         p^.descendant := NIL;
         p^.sibling := NIL;
         p^.lastinlist := NIL;
         p^.ndlablen := 0;
         p^.nbranches := 0;
         p^.nodesize := 1.0;
         p^.len := spanunit;
         p^.spread := pi;
         END; (* newnode *)

      PROCEDURE addnode(anc : nodeptr; VAR linktonode, 
         anclastinlist : nodeptr);
         CONST blank = ' ';
            point = '.';
            comma = ',';
            colon = ':';
            leftp = '(';
            rightp = ')';
            aster = '*';
         VAR node : nodeptr;
            doingdescendants : BOOLEAN;

         PROCEDURE getch(VAR ch : CHAR);
         BEGIN (* getch *)
            IF EOF(treefile)
               THEN BEGIN
                  WRITELN(OUTPUT, '  NO ''*'' AT END OF TREE ',
                     'DESCRIPTION, ''*'' ASSUMED');
                  ch := '*';
                  END
               ELSE BEGIN
                  IF EOLN(treefile) THEN READLN(treefile);
                  READ(treefile, ch);
                  END;
            END; (* getch *)

         PROCEDURE processnodelabel(node : nodeptr);
         BEGIN (* processnodelabel *)
            IF node^.ndlablen=0
               THEN BEGIN
                  labelspresent := TRUE;
                  REPEAT
                     IF node^.ndlablen<maxlablen THEN BEGIN
                        node^.ndlablen := node^.ndlablen+1;
                        node^.ndlabel[node^.ndlablen] := ch;
                        END;
                     getch(ch);
                     UNTIL (ch=blank) OR (ch=leftp) OR (ch=comma) OR
                        (ch=rightp) OR (ch=colon) OR (ch=aster);
                  END
               ELSE REPEAT getch(ch) UNTIL (ch=blank) OR (ch=leftp) 
                  OR (ch=comma) OR (ch=rightp) OR (ch=colon) OR 
                  (ch=aster);
            END; (* processnodelabel *)

         PROCEDURE processlength(node : nodeptr);
            VAR digit, ordzero : INTEGER;
               value, divisor : REAL;
               pointread : BOOLEAN;
         BEGIN (* processlength *)
            ordzero := ORD('0');
            pointread := FALSE;
            value := 0.0;
            divisor := 1.0;
            REPEAT getch(ch) UNTIL ch <> blank;
            digit := ORD(ch)-ordzero;
            WHILE ((digit >= 0) AND (digit <= 9)) OR (ch=point)
               DO BEGIN
               IF ch = point
                  THEN pointread := TRUE
                  ELSE BEGIN
                     value := value*10.0 + digit;
                     IF pointread THEN divisor := divisor*10.0;
                     END;
               getch(ch);
               digit := ORD(ch)-ordzero;
               END;
            IF lengthmode = yes 
               THEN node^.len := (value/divisor)*spanunit;
            END; (* processlength *)

      BEGIN (* addnode *)
         doingdescendants := FALSE;
         newnode(node);
         linktonode := node;
         anclastinlist := node;
         node^.ancestor := anc;
         WHILE ch=blank DO getch(ch);
         WHILE (ch <> aster) AND (((ch <> comma) AND
            (ch <> rightp)) OR doingdescendants) DO BEGIN
            IF ch = leftp
               THEN BEGIN
                  doingdescendants := TRUE;
                  getch(ch);
                  addnode(node, node^.descendant,
                     node^.lastinlist);
                  node^.nbranches := node^.nbranches+1;
                  END
               ELSE IF ch = comma
                  THEN BEGIN
                     getch(ch);
                     addnode(node, node^.lastinlist^.sibling,
                        node^.lastinlist);
                     node^.nbranches := node^.nbranches+1;
                     END
                  ELSE IF ch = rightp
                     THEN BEGIN
                        doingdescendants := FALSE;
                        REPEAT getch(ch) UNTIL ch <> blank;
                        END
                     ELSE IF ch = colon
                        THEN processlength(node)
                        ELSE IF ch <> aster
                           THEN processnodelabel(node);
            WHILE ch = blank DO getch(ch);
            END;
         END; (* addnode *)

   BEGIN (* readtree *)
      count := 0;
      ch := ' ';
      addnode(NIL, root, root);
      READLN(treefile);
      nnodes := count;
      initnodes(root, root);
      FOR i := 1 TO nnodes DO BEGIN
         node := nodelist[i];
         IF node^.ndlablen = 0 THEN node^.nodesize := smallnode;
         IF (node^.ndlablen = 1) AND (node^.ndlabel[1] = '@') THEN
            node^.ndlablen := 0;
         END;
      IF lengthmode = yes THEN
         FOR i := 1 TO nnodes DO BEGIN
            node := nodelist[i];
            IF node <> root THEN node^.len := node^.len +
               (node^.nodesize + node^.ancestor^.nodesize)*radius;
            END;
      END; (* readtree *)

   PROCEDURE calctree(p : nodeptr);
      VAR fitok : BOOLEAN;
         current : nodeptr;

         PROCEDURE fanflip(p, bottom : nodeptr; angle : REAL;
            flip : BOOLEAN);
            VAR current : nodeptr;
         BEGIN (* fanflip *)
            IF p <> NIL THEN BEGIN
               fanflip(p^.descendant, bottom, angle, flip);
               IF p <> bottom THEN fanflip(p^.sibling, bottom,
                                                 angle, flip);
               IF flip
                  THEN BEGIN
                     p^.theta := angle-p^.theta;
                     current := p^.descendant;
                     p^.descendant := p^.lastinlist;
                     WHILE current <> p^.descendant DO BEGIN
                        p^.lastinlist^.sibling := current;
                        p^.lastinlist := p^.lastinlist^.sibling;
                        current := current^.sibling;
                        p^.lastinlist^.sibling := NIL;
                        END;
                     current := p^.left;
                     p^.left := p^.right;
                     p^.right := current;
                     END
                  ELSE p^.theta := angle + p^.theta;
               IF p^.theta < bottom^.ancestor^.left^.theta
                  THEN bottom^.ancestor^.left := p;
               IF p^.theta > bottom^.ancestor^.right^.theta
                  THEN bottom^.ancestor^.right := p;
               END;
            END; (* fanflip *)

      PROCEDURE arrange(p : nodeptr; VAR fitok : BOOLEAN);
         VAR nlong : INTEGER;
            maxheight, angle, angleused, spacing : REAL;
            flip : BOOLEAN;
            previous, current, next : nodeptr;

         PROCEDURE switchbranches(VAR ance, prev, curr, next 
               : nodeptr;
            switch : BOOLEAN);
         BEGIN (* switchbranches *)
            IF switch
               THEN BEGIN
                  IF prev = ance
                     THEN ance^.descendant := next
                     ELSE prev^.sibling := next;
                  curr^.sibling := next^.sibling;
                  next^.sibling := curr;
                  prev := ance;
                  curr := ance^.descendant;
                  next := curr^.sibling;
                  END
               ELSE BEGIN
                  prev := curr;
                  curr := next;
                  next := next^.sibling;
                  END;
            END; (* switchbranches *)

      BEGIN (* arrange *)
         previous := p;
         current := p^.descendant;
         next := current^.sibling;
         WHILE next <> NIL DO
            IF (current^.height < next^.height) OR
               ((current^.height = next^.height) AND
               (current^.width < next^.width))
               THEN switchbranches(p, previous, current, next, doswitch)
               ELSE switchbranches(p, previous, current,
                                                   next, dontswitch);
         nlong := 0;
         current := p^.descendant;
         maxheight := current^.height;
         WHILE current <> NIL DO BEGIN
            IF current^.height >= heightval*maxheight
               THEN nlong := nlong + 1;
            current := current^.sibling;
            END;
         angleused := 0.0;
         current := p^.descendant;
         WHILE current <> NIL DO BEGIN
            angleused := angleused + current^.width;
            current := current^.sibling;
            END;
         spacing := (p^.spread-angleused)/(p^.nbranches+1);
         fitok := spacing >= spaceval;
         current := p^.descendant;
         IF ODD(nlong)
            THEN angle := 0.0
            ELSE angle := spacing/2.0 + current^.right^.theta;
         p^.right := current^.left;
         p^.left := current^.right;
         flip := TRUE;
         fanflip(current, current, angle, flip);
         current := current^.sibling;
         WHILE current <> NIL DO BEGIN
            IF ABS(p^.left^.theta) > ABS(p^.right^.theta)
               THEN angle := p^.right^.theta
                             + spacing + current^.right^.theta
               ELSE angle := p^.left^.theta - spacing
                             - current^.right^.theta;
            flip := angle >= 0.0;
            fanflip(current, current, angle, flip);
            current := current^.sibling;
            END;
         previous := p;
         current := p^.descendant;
         next := current^.sibling;
         WHILE next <> NIL DO
            IF current^.theta > next^.theta
               THEN switchbranches(p, previous, current, next, doswitch)
               ELSE switchbranches(p, previous, current, next,
                                                     dontswitch);
         p^.lastinlist := current;
         END; (* arrange *)

      PROCEDURE scrunch(p, bottom : nodeptr);
      BEGIN (* scrunch *)
         IF p <> NIL THEN BEGIN
            IF (monitor = yes) AND (p = bottom)
               THEN WRITELN(OUTPUT, '    SCRUNCHING AT NODE', p^.c:4);
            scrunch(p^.descendant, bottom);
            IF p <> bottom THEN scrunch(p^.sibling, bottom);
            p^.spread := p^.spread * scrunchval;
            IF p^.spread < scrunchlimit THEN candraw := FALSE;
            END;
         END; (* scrunch *)

      PROCEDURE polartrans(p, bottom : nodeptr; len : REAL);
         VAR pr, ptheta : REAL;
      BEGIN (* polartrans *)
         IF p <> NIL THEN BEGIN
            polartrans(p^.descendant, bottom, len);
            IF p <> bottom THEN polartrans(p^.sibling, bottom, len);
            pr := p^.r;
            ptheta := p^.theta;
            p^.r := SQRT(SQR(len) + SQR(pr)+ 2*len*pr*COS(ptheta));
            p^.theta := ARCTAN(pr*SIN(ptheta)/(pr*COS(ptheta) + len));
            IF p^.r  > bottom^.height THEN bottom^.height := p^.r;
            IF p^.theta > bottom^.right^.theta THEN bottom^.right := p;
            IF p^.theta < bottom^.left^.theta THEN bottom^.left := p;
            END;
         END; (* polartrans *)

   BEGIN (* calctree *)
      IF (p <> NIL) AND candraw THEN BEGIN
         IF monitor = yes THEN WRITELN(OUTPUT, '     AT NODE',p^.c:4);
         REPEAT
            calctree(p^.descendant);
            IF p^.descendant <> NIL
               THEN arrange(p,fitok)
               ELSE fitok := TRUE;
            IF fitok
               THEN BEGIN
                  IF p <> root THEN BEGIN
                     p^.right := p;
                     p^.left := p;
                     polartrans(p, p, p^.len);
                     p^.width := p^.right^.theta - p^.left^.theta;
                     END;
                  END
               ELSE BEGIN
                  current := p^.descendant;
                  WHILE current <> NIL DO BEGIN
                     scrunch(current, current);
                     initnodes(current, current);
                     current := current^.sibling;
                     END;
                  END;
            UNTIL fitok OR NOT candraw;
         IF candraw THEN BEGIN
            IF ABS(p^.right^.theta) > ABS(p^.left^.theta)
               THEN fanflip(p, p, 0.0, TRUE);
            calctree(p^.sibling);
            END;
         END;
      END; (* calctree *)

   PROCEDURE hardwarecoordinates;
      VAR i : INTEGER;
         factor, angle, xmax, xmin, ymax, ymin, thisx, thisy,
         thisrad, xtrans, ytrans : REAL;
         p : nodeptr;
   BEGIN (* hardwarecoordinates *)
      IF rotation <> 0.0 THEN BEGIN
         rotation := rotation*pi/180.0;
         FOR i := 1 TO nnodes DO
            nodelist[i]^.theta := nodelist[i]^.theta + rotation;
         END;
      xmax := -1000000.0;
      xmin := 1000000.0;
      ymax := -1000000.0;
      ymin := 1000000.0;
      FOR i := 1 TO nnodes DO BEGIN
         p := nodelist[i];
         p^.x := p^.r*SIN(p^.theta);
         p^.y := p^.r*COS(p^.theta);
         thisx := p^.x;
         thisy := p^.y;
         thisrad := p^.nodesize*radius;
         IF thisx + thisrad > xmax THEN xmax := thisx + thisrad;
         IF thisx - thisrad < xmin THEN xmin := thisx - thisrad;
         IF thisy + thisrad > ymax THEN ymax := thisy + thisrad;
         IF thisy - thisrad < ymin THEN ymin := thisy - thisrad;
         END;
      xrange := xmax - xmin;
      yrange := ymax - ymin;
      IF sizemode = yes
         THEN BEGIN
            IF ysize/xsize > yrange/xrange
               THEN factor := xsize/xrange
               ELSE factor := ysize/yrange;
            IF 2.0*radius*factor > maxnodesize
               THEN factor := maxnodesize/(2.0*radius);
            END
         ELSE factor := 1.0;
      CASE xoption OF
         right: xtrans := xposition - xmin*factor;
         left: xtrans := xposition - xmax*factor;
         center: xtrans := xposition + (xrange/2.0 - xmax)*factor
         END;
      CASE yoption OF
         above: ytrans := yposition - ymin*factor;
         below: ytrans := yposition - ymax*factor;
         center: ytrans := yposition + (yrange/2.0 - ymax)*factor
         END;
      FOR i := 1 TO nnodes DO BEGIN
         p := nodelist[i];
         p^.x := (p^.x*factor+xtrans)*xunitspercm;
         p^.y := (p^.y*factor+ytrans)*xunitspercm
         END;
      radius := radius * factor * xunitspercm;
      xrange := xrange * factor * xunitspercm;
      yrange := yrange * factor * xunitspercm;
      FOR i := 1 TO 30 DO BEGIN
         angle := i * pi/15.0;
         xcircle[i] := SIN(angle) * radius;
         ycircle[i] := -COS(angle) * radius;
         END;
      END; (* hardwarecoordinates *)

   PROCEDURE loadfont(VAR font : fonttype);
      VAR i, charstart, hersheynum : INTEGER;
         ch : CHAR;
   BEGIN (* loadfont *)
      i := 0;
      WHILE NOT EOF(fontfile) DO BEGIN
         charstart := i+1;
         READ(fontfile, ch, ch, hersheynum,
            font[charstart+2], font[charstart+3]);
         font[charstart+1] := ORD(ch);
         i := charstart+3;
         REPEAT
            IF (i-(charstart+3)) MOD 10=0 THEN READLN(fontfile);
            i := i+1;
            READ(fontfile,font[i]);
            UNTIL ABS(font[i]) >= 10000;
         READLN(fontfile);
         font[charstart] := i+1;
         END;
      font[charstart] := 0;
      WRITELN(OUTPUT, '  FONT REQUIRES', i:5, ' INTEGERS OUT OF',
         fontsize:5, ' AVAILABLE');
      END; (* loadfont *)

   PROCEDURE labelsetup;
      VAR i : INTEGER;
   BEGIN (* labelsetup *)
      longestlablen := 0;
      FOR i := 1 TO nnodes DO
         IF nodelist[i]^.ndlablen > longestlablen
            THEN longestlablen := nodelist[i]^.ndlablen;
      IF longestlablen > 0
         THEN labelheight := 2.0*radius/SQRT(SQR(longestlablen)+1.0)
         ELSE labelspresent := FALSE;
      END; (* labelsetup *)

   PROCEDURE plot(penstatus : penstatustype; xabs, yabs : REAL);
      VAR i, n, xrel, yrel, xinc, yinc, xhigh, yhigh,
            xlow, ylow, xlast, ylast : INTEGER;
         quadrant : CHAR;

      PROCEDURE pout(n : INTEGER);
      BEGIN (* pout *)
         IF n >= 1000
            THEN WRITE(plotfile, n:4)
            ELSE IF n >= 100
               THEN WRITE(plotfile, n:3)
               ELSE IF n >= 10
                  THEN WRITE(plotfile, n:2)
                  ELSE IF n >= 0
                     THEN WRITE(plotfile, n:1)
                     ELSE IF n >= -9
                        THEN WRITE(plotfile, n:2)
                        ELSE IF n >= -99
                           THEN WRITE(plotfile, n:3)
                           ELSE IF n >= -999
                              THEN WRITE(plotfile, n:4)
                              ELSE WRITE(plotfile, n:5);
         END; (* pout *)

      BEGIN (* plot *)
         yabs := yabs * aspectratio;
         CASE plotter OF
            calcomp: BEGIN
               IF penstatus = pendown
                  THEN WRITE(plotfile,'H')
                  ELSE WRITE(plotfile,'D');
               xrel := ROUND(xabs) - xnow;
               yrel := ROUND(yabs) - ynow;
               xnow := ROUND(xabs);
               ynow := ROUND(yabs);
               IF xrel > 0
                  THEN IF yrel > 0
                     THEN quadrant := 'P'
                     ELSE quadrant := 'T'
                  ELSE IF yrel > 0
                     THEN quadrant := 'X'
                     ELSE quadrant := '1';
               xrel := ABS(xrel);
               yrel := ABS(yrel);
               IF xrel > yrel
                  THEN n := (xrel DIV 255) + 1
                  ELSE n := (yrel DIV 255) + 1;
               xinc := xrel DIV n;
               yinc := yrel DIV n;
               xlast := xrel MOD n;
               ylast := yrel MOD n;
               xhigh := xinc DIV 16;
               yhigh := yinc DIV 16;
               xlow := xinc MOD 16;
               ylow := yinc MOD 16;
               FOR i := 1 TO n DO
                  WRITE(plotfile, quadrant, cchex[xhigh], cchex[xlow],
                     cchex[yhigh], cchex[ylow]);
               IF (xlast <> 0) OR (ylast <> 0)
                  THEN WRITE(plotfile, quadrant, cchex[0], cchex[xlast],
                     cchex[0], cchex[ylast]);
               END;
            tektronix: BEGIN
               IF penstatus = penup
                  THEN WRITE(plotfile, CHR(29));
               xnow := ROUND(xabs);
               ynow := ROUND(yabs);
               xhigh := xnow DIV 32;
               yhigh := ynow DIV 32;
               xlow := xnow MOD 32;
               ylow := ynow MOD 32;
               IF yhigh <> oldyhigh THEN WRITE(plotfile,
                                                 CHR(yhigh+32));
               IF (ylow <> oldylow) OR (xhigh <> oldxhigh)
                  THEN WRITE(plotfile, CHR(ylow+96));
               IF xhigh <> oldxhigh THEN WRITE(plotfile,
                                                 CHR(xhigh+32));
               WRITE(plotfile, CHR(xlow+64));
               oldxhigh := xhigh;
               oldxlow := xlow;
               oldyhigh := yhigh;
               oldylow := ylow;
               END;
            hp7470: BEGIN
               IF penstatus=pendown
                  THEN WRITE(plotfile, 'PD')
                  ELSE WRITE(plotfile, 'PU');
               pout(ROUND(xabs));
               WRITE(plotfile, ',');
               pout(ROUND(yabs));
               WRITELN(plotfile, ';');
               END;
            radioshack: BEGIN
               IF penstatus = pendown
                  THEN WRITE(plotfile, 'D')
                  ELSE WRITE(plotfile, 'M');
               pout(ROUND(xabs));
               WRITE(plotfile, ',');
               pout(ROUND(yabs));
               WRITELN(plotfile);
               END;
            other : BEGIN
               (* code for a pen move on a new plotter
                  goes here *)
               END
            END;
         END; (* plot *)

      PROCEDURE drawtree(p : nodeptr);

         PROCEDURE drawinternode(x1, y1, x2, y2, ndsize1,
                                              ndsize2 : REAL);
            VAR slope, xdiff, ydiff, xstart, ystart, xend,
                   yend : REAL;

            FUNCTION trimend(thisend, farend, amount : REAL) : REAL;
            BEGIN (* trimend *)
               IF thisend > farend
                  THEN trimend := thisend - amount
                  ELSE trimend := thisend + amount;
               END; (* trimend *)

         BEGIN (* drawinternode *)
            IF x1 = x2
               THEN BEGIN
                  xdiff := 0.0;
                  ydiff := radius;
                  END
               ELSE IF y1 = y2
                  THEN BEGIN
                     xdiff := radius;
                     ydiff := 0.0;
                     END
                  ELSE BEGIN
                     slope := (y2 - y1)/(x2 - x1);
                     xdiff := radius/SQRT(SQR(slope) + 1.0);
                     ydiff := radius/SQRT((1.0/SQR(slope)) + 1.0);
                     END;
            xstart := trimend(x1, x2, xdiff*ndsize1);
            ystart := trimend(y1, y2, ydiff*ndsize1);
            xend := trimend(x2, x1, xdiff*ndsize2);
            yend := trimend(y2, y1, ydiff*ndsize2);
            plot(penup, xstart, ystart);
            plot(pendown, xend, yend);
            END; (* drawinternode *)

         PROCEDURE drawnode(x, y, ndsize : REAL);
            VAR i : INTEGER;
         BEGIN (* drawnode *)
            plot(penup, x, y - radius*ndsize);
            FOR i := 1 TO 30 DO plot(pendown, x+xcircle[i]*ndsize,
               y+ycircle[i]*ndsize);
            END; (* drawnode *)

         PROCEDURE plotlabel(p : nodeptr);
            TYPE pentype = (treepen, labelpen);
            VAR textlen, compress, xlab, ylab, x, y : REAL;

            PROCEDURE changepen(pen : pentype);
            BEGIN (* changepen *)
               CASE pen OF
                  treepen: CASE plotter OF
                     hp7470: WRITELN(plotfile, 'SP1;');
                     other : BEGIN
                        (* code for a new plotter goes here *)
                        END
                     END;
                  labelpen: CASE plotter OF
                     hp7470: WRITELN(plotfile, 'SP2;');
                     other : BEGIN
                        (* code for a new plotter goes here *)
                        END
                     END;
                  END;
               END; (* changepen *)

            PROCEDURE plottext(pstring : plotstring; nchars : INTEGER;
               height, compress : REAL; VAR x, y : REAL;
               slope : REAL; font : fonttype);
               CONST xstart = 10;
                  ystart = 35;
               VAR i, j, code : INTEGER;
                  sinslope, cosslope : REAL;

               PROCEDURE plotchar(VAR place : INTEGER);
                  VAR coord : INTEGER;
                     heightfont, widthfont, xfactor, yfactor, xfont,
                     yfont, xplot, yplot : REAL;
                     penstatus : penstatustype;
               BEGIN (* plotchar *)
                  heightfont := font[place+2];
                  widthfont := font[place+3];
                  yfactor := height/heightfont;
                  xfactor := yfactor*compress;
                  place := place+3;
                  REPEAT
                     place := place+1;
                     coord := font[place];
                     IF coord > 0
                        THEN penstatus := pendown
                        ELSE penstatus := penup;
                     coord := ABS(coord);
                     coord := coord MOD 10000;
                     xfont := ((coord DIV 100) - xstart)*xfactor;
                     yfont := ((coord MOD 100) - ystart)*yfactor;
                     IF slope = 0.0
                        THEN BEGIN
                           xplot := x + xfont;
                           yplot := y + yfont;
                           END
                        ELSE BEGIN
                           xplot := x + xfont*cosslope
                                       + yfont*sinslope;
                           yplot := y - xfont*sinslope
                                       + yfont*cosslope;
                           END;
                     plot(penstatus, xplot, yplot);
                     UNTIL ABS(font[place]) >= 10000;
                  x := xplot;
                  y := yplot;
                  END; (* plotchar *)

            BEGIN (* plottext *)
               IF slope <> 0.0 THEN BEGIN
                  sinslope := SIN(slope);
                  cosslope := COS(slope);
                  END;
               FOR i := 1 TO nchars DO BEGIN
                  code := ORD(pstring[i]);
                  j := 1;
                  WHILE (font[j+1] <> code) AND (font[j] <> 0) DO
                     j := font[j];
                  IF font[j+1] = code THEN plotchar(j);
                  END;
               END; (* plottext *)

            FUNCTION lengthtext(pstring : plotstring; nchars : INTEGER;
               height, compress : REAL; font : fonttype) : REAL;
               VAR i, j, code, coord : INTEGER;
                  sumlength, heightfont, widthfont, yfactor : REAL;
            BEGIN (* lengthtext *)
               sumlength := 0.0;
               FOR i := 1 TO nchars DO BEGIN
                  code := ORD(pstring[i]);
                  j := 1;
                  WHILE (font[j+1] <> code) AND (font[j] <> 0) DO
                     j := font[j];
                  IF font[j+1] = code THEN BEGIN
                     heightfont := font[j+2];
                     widthfont := font[j+3];
                     sumlength := sumlength + (height/heightfont)
                           *widthfont;
                     END;
                  END;
                  lengthtext := sumlength*compress;
               END; (* lengthtext *)

         BEGIN (* plotlabel *)
            IF penchange = yes THEN changepen(labelpen);
            compress := 1.0;
            textlen := lengthtext(p^.ndlabel, p^.ndlablen,
               labelheight, compress, font);
            If textlen > p^.ndlablen*labelheight THEN BEGIN
               compress := p^.ndlablen*labelheight/textlen;
               textlen := p^.ndlablen*labelheight;
               END;
            xlab := -textlen*labelsize/2.0;
            ylab := -labelheight*labelsize/2.0;
            x := p^.x + xlab*COS(rotation) + ylab*SIN(rotation);
            y := p^.y - xlab*SIN(rotation) + ylab*COS(rotation);
            plottext(p^.ndlabel, p^.ndlablen, labelheight*labelsize,
                         compress, x, y, rotation, font);
            IF penchange = yes THEN changepen(treepen);
            END; (* plotlabel *)

      BEGIN (* drawtree *)
         IF p <> NIL THEN BEGIN
            IF p^.ancestor <> NIL THEN drawinternode(p^.ancestor^.x,
               p^.ancestor^.y, p^.x, p^.y, p^.ancestor^.nodesize,
               p^.nodesize);
            drawnode(p^.x, p^.y, p^.nodesize);
            IF labelspresent AND (p^.ndlablen > 0) THEN plotlabel(p);
            drawtree(p^.descendant);
            drawtree(p^.sibling);
            END;
         END; (* drawtree *)

   PROCEDURE initplotter;
   BEGIN (* initplotter *)
      CASE plotter OF
         calcomp: BEGIN
            cchex[0] := 'C'; cchex[1] := 'D'; cchex[2] := 'H';
            cchex[3] := 'L'; cchex[4] := 'P'; cchex[5] := 'T';
            cchex[6] := 'X'; cchex[7] := '1'; cchex[8] := '5';
            cchex[9] := '9'; cchex[10] := '/'; cchex[11] := '=';
            cchex[12] := '#'; cchex[13] := '"'; cchex[14] := '''';
            cchex[15] := '@';
            xnow := 0;
            ynow := 0;
            WRITE(plotfile, 'CCCCCCCCCC');
            END;
         tektronix: BEGIN
            oldxhigh := -1;
            oldxlow := -1;
            oldyhigh := -1;
            oldylow := -1;
            WRITE(plotfile, CHR(27), CHR(12));
            END;
         hp7470: WRITELN(plotfile, 'IN;SP1;VS10.0;');
         radioshack: BEGIN
            WRITELN(plotfile, CHR(18));
            plot(penup, 0.0, -(yrange+40.0));
            WRITELN(plotfile, 'I');
            END;
         other : BEGIN
            (* initialization code for a new plotter goes here *)
            END
         END;
      END; (* initplotter *)

      PROCEDURE finishplotter;
      BEGIN (* finishplotter *)
         CASE plotter OF
            calcomp: plot(penup, 0.0, yrange+50.0);
            hp7470: BEGIN
               plot(penup,0.0,0.0);
               WRITELN(plotfile,'SP;');
               END;
            tektronix: WRITELN(plotfile);
            radioshack: plot(penup,0.0,0.0);
            other : BEGIN
               (* termination code for a new plotter goes here *)
               END
            END;
         END; (* finishplotter *)

   PROCEDURE garbagestack;
      VAR i : INTEGER;
   BEGIN (* garbagestack *)
      FOR i := 1 TO nnodes DO BEGIN
         nodelist[i]^.descendant := garbage;
         garbage := nodelist[i];
         END;
      END; (* garbagestack *)

BEGIN (* Plotree *)
   WRITELN(OUTPUT, ' PLOTREE VERSION 1.0');
   WRITELN(OUTPUT, ' DISTRIBUTED WITH PHYLIP 2.6');
   WRITELN(OUTPUT, ' READING OF PARAMETERS BEGINS');
   RESET(parmfile);
   initparms;
   WRITELN(OUTPUT, ' READING OF PARAMETERS COMPLETED');
   IF parmsok THEN BEGIN
      RESET(treefile);
      WHILE NOT EOF(treefile) DO BEGIN
         candraw := TRUE;
         radius := diameter/2.0;
         WRITELN(OUTPUT, ' READING OF TREE BEGINS');
         readtree;
         WRITELN(OUTPUT, ' READING OF TREE COMPLETED');
         WRITELN(OUTPUT, ' CALCULATION OF TREE BEGINS');
         calctree(root);
         WRITELN(OUTPUT, ' CALCULATION OF TREE COMPLETED');
         IF candraw
            THEN BEGIN
               hardwarecoordinates;
               IF labelspresent THEN BEGIN
                  labelsetup;
                  IF NOT(fontloaded) AND labelspresent THEN BEGIN
                     WRITELN(OUTPUT, ' LOADING OF FONT BEGINS');
                     RESET(fontfile);
                     loadfont(font);
                     WRITELN(OUTPUT, ' LOADING OF FONT COMPLETED');
                     fontloaded := TRUE;
                     END;
                  END;
               WRITELN(OUTPUT, ' DRAWING OF TREE BEGINS');
               REWRITE(plotfile);
               initplotter;
               drawtree(root);
               finishplotter;
               WRITELN(OUTPUT, ' DRAWING OF TREE COMPLETED');
               END
            ELSE BEGIN
               WRITELN(OUTPUT, ' TREE CANNOT BE DRAWN WITH CURRENT ',
                         'PARAMETERS');
               WRITELN(OUTPUT, '  NODE DIAMETER MUST BE SMALLER OR ',
                         'INTERNODE LENGTH LARGER');
               END;
         garbagestack;
         END;
      END;
   (* CLOSE(plotfile);   may have to change this *)
   WRITELN(OUTPUT, ' EXECUTION TERMINATED');
   END. (* plotree *)
