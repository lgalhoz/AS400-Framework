     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Biblitoeca.:
     H* Descrição..:
     H*
     H*  This program implements a simple XML DOM Builder.
     H*  Written by Andrew Young
     H*==================================================================
     D MAX_NODES       C                   65535
     D
     D NODE_DEF        DS
     D   nodeId                       5U 0 INZ(1)
     D   nodeParent                   5U 0 INZ(0)
     D   nodeType                     1A   INZ('E')
     D   nodeName@                     *   INZ(*NULL)
     D   nodeNameLen                  5U 0 INZ(0)
     D   nodeValue@                    *   INZ(*NULL)
     D   nodeValLen                   5U 0 INZ(0)
     D
     D NODE_LIST       DS
     D nodes                           *   DIM(MAX_NODES)
     D nodeEnd                        5U 0 INZ(1)
     D
     D*-------------------------------------------------------
     D $BuildXML       PR         65535A   VARYING
     D                                 *   VALUE
     D
     D $CreateNode     PR              *
     D                                5U 0 VALUE
     D
     D $SetNodeName    PR             3U 0
     D                                 *   VALUE
     D                            65535A   VALUE VARYING
     D
     D $GetNodeName    PR         65535A   VARYING
     D                                 *   VALUE
     D
     D $SetNodeValue   PR             3U 0
     D                                 *   VALUE
     D                            65535A   VALUE VARYING
     D
     D $GetNodeValue   PR         65535A   VARYING
     D                                 *   VALUE
     D
     D $GetNode        PR              *
     D                                5U 0 VALUE
     D
     D $GetParentNode  PR              *
     D                                 *   VALUE
     D
     D $NodeTest       PR             3U 0
     D
     D $PrintNode      PR             3U 0
     D                                 *   VALUE
     D
     D $DisplayString  PR             3U 0
     D                            65535A   VARYING VALUE
     D
     D $EscapeXML      PR         65535A   VARYING
     D                            65535A   VARYING VALUE
     D
     D $Cleanup        PR
     D*---------------------------------------------------
     D fd              S             10i 0
     D*=========================================================
      /FREE
       fd = open('/ifstest/ch5_file.txt':
            O_TRUNC+O_CREAT+O_WRONLY:
            S_IWUSR+S_IRUSR+S_IRGRP+S_IROTH);
       if (fd > *ZEROS);


       endif;


       // $nodeTest();
       // $cleanup();
       *INLR = *ON;
       RETURN;
      /END-FREE

     P*======================================================================
     P* Função.....: $CleanUp
     P* Descrição..:
     P*======================================================================
     P $CleanUp        B
     D
     D*=> Campos de Trabalho
     D i               S             10I 0 INZ(1)
     D ret             S              3U 0
     D node@           S               *
     D node            DS                  LIKEDS(NODE_DEF) BASED(node@)
     D*======================================================================
      /FREE
       DOW i < nodeEnd;
         node@ = $getNode(i);
         IF node@ <> *NULL;
           IF node.nodeName@ <> *NULL;
             DEALLOC node.nodeName@;
             node.nodeName@ = *NULL;
           ENDIF;
           IF node.nodeValue@ <> *NULL;
             DEALLOC node.nodeValue@;
             node.nodeValue@ = *NULL;
           ENDIF;
           DEALLOC node@;
           node@ = *NULL;
         ENDIF;
         i = i + 1;
       ENDDO;
      /END-FREE
     P $CleanUp        E

     P*======================================================================
     P* Função.....: $NodeTest
     P* Descrição..:
     P*======================================================================
     P $NodeTest       B
     D                 PI             3U 0
     D
     D*=> Campos de Trabalho
     D node@           S               *
     D node            DS                  LIKEDS(NODE_DEF) BASED(node@)
     D nodeIndex       S              5U 0
     D ret             S              3U 0
     D*======================================================================
      /FREE
       node@ = $CreateNode(0);
       ret = $setNodeName(node@:'documents');

       node@ = $CreateNode(node.nodeId);
       ret = $setNodeName(node@:'document');

       node@ = $CreateNode(node.nodeId);
       ret = $setNodeName(node@:'name');
       node.nodeType = 'A';
       ret = $setNodeValue(node@:'My First Document');

       node@ = $getParentNode(node@);

       node@ = $CreateNode(node.nodeId);
       node.nodeType = 'T';
       ret = $setNodeValue(node@:'Some text.');

       node@ = $getParentNode(node@);
       node@ = $getParentNode(node@);

       node@ = $CreateNode(node.nodeId);
       ret = $setNodeName(node@:'document');

       node@ = $CreateNode(node.nodeId);
       node.nodeType = 'A';
       ret = $setNodeName(node@:'name');
       ret = $setNodeValue(node@:'A Second Document <>&"&"><');

       node@ = $getParentNode(node@);

       node@ = $CreateNode(node.nodeId);
       ret = $setNodeName(node@:'field');

       node@ = $CreateNode(node.nodeId);
       node.nodeType = 'A';
       ret = $setNodeName(node@:'name');
       ret = $setNodeValue(node@:'url');

       node@ = $getParentNode(node@);

       node@ = $CreateNode(node.nodeId);
       node.nodeType = 'T';
       ret = $setNodeValue(node@:'http://www.google.com/');

       node@ = $getParentNode(node@);
       node@ = $getParentNode(node@);

       node@ = $CreateNode(node.nodeId);
       ret = $setNodeName(node@:'description');

       node@ = $CreateNode(node.nodeId);
       node.nodeType = 'T';
       ret = $setNodeValue(node@:'Here is a link: ');

       node@ = $getParentNode(node@);

       node@ = $CreateNode(node.nodeId);
       $setNodeName(node@:'a');

       node@ = $CreateNode(node.nodeId);
       node.nodeType = 'A';
       $SetNodeName(node@:'href');
       $setNodeValue(node@:'http://www.google.com/');

       node@ = $getParentNode(node@);

       node@ = $CreateNode(node.nodeId);
       node.nodeType = 'T';
       $setNodeValue(node@:'Google');

       node@ = $getParentNode(node@);

       node@ = $getParentNode(node@);

       node@ = $CreateNode(node.nodeId);
       node.nodeType = 'T';
       $setNodeValue(node@:', and here is some text that needs to be escaped: '+
           '"<>"&><&');

       nodeIndex = 1;
       DOW nodeIndex < nodeEnd;
         node@ = $getNode(nodeIndex);
         ret = $printNode(node@);
         nodeIndex = nodeIndex + 1;
       ENDDO;

       ret = $displayString($BuildXML($GetNode(1)));

       RETURN 0;
      /END-FREE
     P $NodeTest       E

     P*======================================================================
     P* Função.....: $PrintNode
     P* Descrição..: This routine prints out the value of the current node
     P*======================================================================
     P $PrintNode      B
     D                 PI             3U 0
     D
     D*=> Campos de Trabalho
     D node@                           *   VALUE
     D node            DS                  LIKEDS(NODE_DEF) BASED(node@)
     D i               S              3U 0
     D*======================================================================
      /FREE
       i = $displayString('Node: ' + %char(node.nodeId));
       i = $displayString('  Parent: ' + %char(node.nodeParent));
       i = $displayString('    Type: ' + node.nodeType);
       i = $displayString('    Name: ' + $getNodeName(node@));
       i = $displayString('   Value: ' + $getNodeValue(node@));
       RETURN 0;
      /END-FREE
     P $PrintNode      E

     P*======================================================================
     P* Função.....: $DisplayString
     P* Descrição..: This routine write the output to standard out using DSPLY.
     P*======================================================================
     P $DisplayString  B
     D                 PI             3U 0
     D output                     65535A   VARYING VALUE
     D
     D*==> Campos de Trabalho
     D message         S             52A   VARYING
     D i               S             10I 0 INZ(0)
     D j               S             10I 0 INZ(0)
     D*======================================================================
      /FREE
       i = 1;
       j = %len(%trim(output));
       DOW i < j;
         message = %subst(output:i);
         DSPLY message;
         i = i + 52;
       ENDDO;
       RETURN 0;
      /END-FREE
     P $DisplayString  E

     P*======================================================================
     P* Função.....: $GetNode
     P* Descrição..:
     P*======================================================================
     P $GetNode        B
     D                 PI              *
     D nodeIndex                      5U 0 VALUE
     D*======================================================================
      /FREE
       RETURN nodes(nodeIndex);
      /END-FREE
     P $GetNode        E

     P*======================================================================
     P* Função.....: $GetParentNode
     P* Descrição..:
     P*======================================================================
     P $GetParentNode  B
     D                 PI              *
     D node@                           *   VALUE
     D
     D*=> Campos de Trabalho
     D node            DS                  LIKEDS(NODE_DEF) BASED(node@)
     D parentNode@     S               *
     D*======================================================================
      /FREE
       IF node.nodeParent > 0;
         parentNode@ = $getNode(node.nodeParent);
       ENDIF;
       RETURN parentNode@;
      /END-FREE
     P $GetParentNode  E

     P*======================================================================
     P* Função.....: $CreateNode
     P* Descrição..:
     P*======================================================================
     P $CreateNode     B
     D                 PI              *
     D parentIndex                    5U 0 VALUE
     D
     D*==> Campos de Trabalho
     D nodeIndex       S              5U 0
     D node@           S               *
     D node            DS                  LIKEDS(NODE_DEF) BASED(node@)
     D i               S              3U 0
     D*======================================================================
      /FREE
       node@ = %alloc(%size(NODE_DEF));
       node.nodeId = nodeEnd;
       node.nodeType = 'E';
       node.nodeParent = parentIndex;
       nodes(nodeEnd) = node@;
       nodeEnd = nodeEnd + 1;
       RETURN node@;
      /END-FREE
     P $CreateNode     E

     P*======================================================================
     P* Função.....: $SetNodeName
     P* Descrição..:
     P*======================================================================
     P $SetNodeName    B
     D                 PI             3U 0
     D node@                           *   VALUE
     D value                      65535A   VALUE VARYING
     D
     D*==> Campos de Trabalho
     D node            DS                  LIKEDS(NODE_DEF) BASED(node@)
     D i               S              3U 0
     D*======================================================================
      /FREE
       IF node.nodeName@ <> *NULL;
         // Deallocate currently used space
         DEALLOC node.nodeName@;
         node.nodeName@ = *NULL;
       ENDIF;
       // Allocate new space
       node.nodeNameLen = %len(value);
       node.nodeName@ = %alloc(node.nodeNameLen+1);
       %str(node.nodeName@:node.nodeNameLen+1) =
           %subst(value:1:node.nodeNameLen);
       RETURN 0;
      /END-FREE
     P $SetNodeName    E

     P*======================================================================
     P* Função.....: $GetNodeName
     P* Descrição..:
     P*======================================================================
     P $GetNodeName    B
     D                 PI         65535A   VARYING
     D node@                           *   VALUE
     D
     D*==> Campos de Trabalho
     D node            DS                  LIKEDS(NODE_DEF) BASED(node@)
     D value           S          65535A   VARYING
     D i               S              3U 0
     D*======================================================================
      /FREE
       IF node.nodeName@ <> *NULL;
         value = %str(node.nodeName@:node.nodeNameLen+1);
       ENDIF;
       RETURN value;
      /END-FREE
     P $GetNodeName    E

     P*======================================================================
     P* Função.....: $SetNodeValue
     P* Descrição..:
     P*======================================================================
     P $SetNodeValue   B
     D                 PI             3U 0
     D node@                           *   VALUE
     D value                      65535A   VALUE VARYING
     D
     D*==> Campos de Trabalho
     D node            DS                  LIKEDS(NODE_DEF) BASED(node@)
     D i               S              3U 0
     D*======================================================================
      /FREE
       IF node.nodeValue@ <> *NULL;
         // Deallocate currently used space
         DEALLOC node.nodeValue@;
         node.nodeValue@ = *NULL;
       ENDIF;
       // Allocate new space
       node.nodeValLen = %len(value);
       node.nodeValue@ = %alloc(node.nodeValLen+1);
       %str(node.nodeValue@:node.nodeValLen+1) =
           %subst(value:1:node.nodeValLen);
       RETURN 0;
      /END-FREE
     P $SetNodeValue   E

     P*======================================================================
     P* Função.....: $GetNodeValue
     P* Descrição..:
     P*======================================================================
     P $GetNodeValue   B
     D                 PI         65535A   VARYING
     D node@                           *   VALUE
     D
     D*==> Campos de Trabalho
     D node            DS                  LIKEDS(NODE_DEF) BASED(node@)
     D value           S          65535A   VARYING
     D*======================================================================
      /FREE
       IF node.nodeValue@ <> *NULL;
         value = %str(node.nodeValue@:node.nodeValLen+1);
       ENDIF;
       RETURN value;
      /END-FREE
     P $GetNodeValue   E

     P*======================================================================
     P* Função.....: $BuildXML
     P* Descrição..:
     P*======================================================================
     P $BuildXML       B
     D                 PI         65535A   VARYING
     D node@                           *   VALUE
     D
     D*==> Campos de Trabalho
     D childNode@      S               *
     D outp            S          65535A   VARYING
     D i               S             10I 0
     D node            DS                  LIKEDS(NODE_DEF) BASED(node@)
     D childNode       DS                  LIKEDS(NODE_DEF) BASED(childNode@)
     D*======================================================================
      /FREE
       // This function writes out the XML structure for the current node and
       //     its children.
       IF node.nodeType = 'E';
         // Write start of element
         outp = outp + '<' + %trim($getNodeName(node@));
         // Look for child attributes
         i = 1;
         DOW i < nodeEnd;
           childNode@ = $getNode(i);
           IF childNode.nodeType = 'A' AND childNode.nodeParent = node.nodeId;
             outp = outp + ' ' + %trim($getNodeName(childNode@)) + '="' +
                 $escapeXML($getNodeValue(childNode@)) + '"';
           ENDIF;
           i = i + 1;
         ENDDO;
         outp = outp + '>';
         // Look for child elements and text nodes
         i = 1;
         DOW i < nodeEnd;
           childNode@ = $getNode(i);
           IF childNode.nodeParent = node.nodeId;
             IF childNode.nodeType = 'T';
               outp = outp + $escapeXML($getNodeValue(childNode@));
             ELSEIF childNode.nodeType = 'E';
               outp = outp + $BuildXML(childNode@);
             ENDIF;
           ENDIF;
           i = i + 1;
         ENDDO;
         outp = outp + '';
       ENDIF;  // node.nodeType = 'E'

       RETURN outp;
      /END-FREE
     P $BuildXML       E

     P*======================================================================
     P* Função.....: $EscapeXML
     P* Descrição..:
     P*======================================================================
     P $EscapeXML      B
     D                 PI         65535A   VARYING
     D input                      65535A   VARYING VALUE
     D
     D*==> Campos de Trabalho
     D inputSize       S              5U 0 INZ(0)
     D i               S              5U 0 INZ(1)
     D currentChar     S              1A   INZ(*BLANKS)
     D outputChar      S              6A   VARYING
     D output          S          65535A   VARYING
     D*======================================================================
      /FREE
       inputSize = %len(input);
       DOW i <= inputSize;
         currentChar = %subst(input:i:1);
         outputChar = currentChar;
         IF currentChar = '&';
           outputChar = '&amp;';
         ELSEIF currentChar = '<';
           outputChar = '&lt;';
         ELSEIF currentChar = '>';
           outputChar = '&gt;';
         ELSEIF currentChar = '"';
           outputChar = '&quot;';
         ENDIF;
         output = output + outputChar;
         i = i + 1;
       ENDDO;
       RETURN output;
      /END-FREE
     P $EscapeXML      E
