      **************************************************************    00030000
ZX    * PAUL FAIT                                                       00040000
ZX    * ZX/ZX/ZX HOST0????? WR39325                                     00050000
ZX    *          NEW WEB COPYBOOK FOR IIB (DE26001)                     00060000
ZX    *          DECOMM + DECUS + DERETURN                              00070000
      **************************************************************    00190000
WEB    01  WEB-DECUS-PARAMETERS.                                        00210001
           04  DE-COMM-AREA.                                            00220000
               05  DE-FIRM               PIC 9(3).                      00230000
               05  DE-SUB                PIC 9(3).                      00240000
               05  DE-REQ.                                              00250000
                   10  DE-APPLICATION    PIC X(4).                      00260000
                       88  APPL-ACCT     VALUE 'ACCT'.                  00270000
                       88  APPL-FUND     VALUE 'FUND'.                  00280000
                       88  APPL-SPHD     VALUE 'SPHD'.                  00290000
100897                 88  APPL-SHIP     VALUE 'SHIP'.                  00300000
                       88  APPL-IRAS     VALUE 'IRAS'.                  00310000
                       88  APPL-VALU     VALUE 'VALU'.                  00320000
                       88  APPL-DESC     VALUE 'DESC'.                  00330000
060998                 88  APPL-VICE     VALUE 'VICE'.                  00340000
                       88  APPL-SACH     VALUE 'SACH'.                  00350000
                       88  APPL-PIPS     VALUE 'PIPS'.                  00360000
                       88  APPL-DOCA     VALUE 'DOCA'.                  00370000
012099                 88  APPL-HOLD     VALUE 'HOLD'.                  00380000
122100                 88  APPL-WRAP     VALUE 'WRAP'.                  00390000
021501                 88  APPL-BORD     VALUE 'BORD'.                  00400000
080201                 88  APPL-MADD     VALUE 'MADD'.                  00410000
032102                 88  APPL-SPAD     VALUE 'SPAD'.                  00420000
041802                 88  APPL-CUST     VALUE 'CUST'.                  00430000
071802                 88  APPL-AIRE     VALUE 'AIRE'.                  00440000
020803                 88  APPL-HAPI     VALUE 'HAPI'.                  00450000
                   10  DE-FUNCTION       PIC X(4).                      00460000
                       88  FUNC-ADD      VALUE 'ADD '.                  00470000
                       88  FUNC-EDIT     VALUE 'EDIT'.                  00480000
                       88  FUNC-CORR     VALUE 'CORR'.                  00490000
                       88  FUNC-VOID     VALUE 'VOID'.                  00500000
                       88  FUNC-INQ      VALUE 'INQ '.                  00510000
041412                 88  FUNC-FIRST    VALUE 'IFUN'.                  00520000
                       88  FUNC-FRWD     VALUE 'FRWD'.                  00530000
                       88  FUNC-LAST     VALUE 'LAST'.                  00540000
041412                 88  FUNC-CLOSE    VALUE 'CFUN'.                  00550000
111012                 88  FUNC-WASH-ADD VALUE 'WASH'.                  00560000
111012                 88  FUNC-WASH-REM VALUE 'WREM'.                  00570000
               05  DE-USER-INFO.                                        00580000
                   10  DE-SECURITY-LEVEL     PIC XX.                    00590000
                   10  DE-ERROR-CODE         PIC 9(4).                  00600000
                   10  DE-USER.                                         00610000
                       15  DE-TERMID         PIC X(10).                 00620000
                       15  DE-USERID         PIC X(10).                 00630000
040816                 15  FILLER REDEFINES DE-USERID.                  00640000
040816                     20  DE-EMPLOYEE-NO PIC 9(5).                 00650000
040816                     20  FILLER        PIC X(5).                  00660000
                   10  DE-DEPT               PIC X(4).                  00670000
                   10  DE-WIRE-CODE          PIC X(4).                  00680000
               05  DE-MESSAGE                PIC X(25).                 00690000
               05  DE-ERROR-TO-TERM-SW       PIC X(1).                  00700000
                   88  DE-SEND-ERROR-TO-TERM VALUE 'Y' SPACES.          00710000
                   88  DE-DONT-SEND-ERROR-TO-TERM                       00720000
                                             VALUE 'N'.                 00730000
               05  DE-ERROR-OPTION           PIC X(1).                  00740000
                   88  DE-RETURN-ERROR-MSG-TO-CALLER                    00750000
                                            VALUE 'R' SPACES.           00760000
                   88  DE-HANDLE-ERROR-IN-SUBROUTINE                    00770000
                                            VALUE 'S'.                  00780000
110499         05  DE-TRANS-RID              PIC X(12).                 00790000
110499         05  DE-TRANS-ORIGIN           PIC X(5).                  00800000
               05  DE-ERROR-RETURN-SUB       PIC 9(3).                  00810000
062002         05  DE-ORIGINATOR             PIC X(8).                  00820000
050604         05  DE-REQUEST-SOURCE-IND     PIC X(1).                  00830000
050604             88  DE-BETALINK-REQ      VALUE 'A'.                  00840000
050604             88  DE-BLSERVER-REQ      VALUE 'B'.                  00850000
050604             88  DE-THIN-CLIENT-REQ   VALUE 'C'.                  00860000
050604             88  DE-PUBLIC-REQ        VALUE 'D'.                  00870000
081410             88  DE-CONVERSION-REQ    VALUE 'V'.                  00880000
060304         05  DE-MAX-BUFFER-IND         PIC X.                     00890000
081410         05  DE-FILE-OR-DB-MODE        PIC X.                     00900000
081410             88  DE-FILE-MODE         VALUE 'F'.                  00910000
081410             88  DE-DB-MODE           VALUE 'D' ' '.              00920000
040816         05  DE-USER-ID-TYPE           PIC X(2).                  00930000
040816         05  FILLER                    PIC X(62).                 00940000
               05  DE-LAST-SET-ID.                                      00950000
                   10  DE-LAST-SET-RID   PIC X(22).                     00960000
                   10  DE-LAST-SET-URI   PIC X(11).                     00970000
                   10  DE-LAST-SET-FILL  PIC X(12).                     00980000
               05  DE-KEY-AREA.                                         00990000
                   10  DE-SEARCH-FORMAT  PIC X.                         01000000
                   10  DE-KEY            PIC X(40).                     01010000
                                                                        01020000
           04  DE-CUS-KEY-AREA                   PIC X(50).             01030000
           04  FILLER REDEFINES DE-CUS-KEY-AREA.                        01040000
               05  CUS-KEY-ACCT-NO               PIC 9(8).              01050000
           04  FILLER REDEFINES DE-CUS-KEY-AREA.                        01060000
               05  CUS-KEY-CUSTOMER-ID           PIC 9(18).             01070000
           04  FILLER REDEFINES DE-CUS-KEY-AREA.                        01080000
               05  CUS-KEY-NAME-TYPE             PIC X(1).              01090000
               05  CUS-KEY-NAME                  PIC X(49).             01100000
           04  FILLER REDEFINES DE-CUS-KEY-AREA.                        01110000
               05  CUS-KEY-TIN-NO                PIC X(11).             01120000
           04  DE-CUSTOMER-LAYOUT.                                      01130000
               05  DE-CUS-CUSTOMER-ID            PIC 9(18).             01140000
               05  DE-CUS-ACCT-NO                PIC 9(8).              01150000
               05  DE-CUS-REP-REP-NO             PIC X(4).              01160000
               05  DE-CUS-DATE-IND               PIC X(1).              01170000
               05  DE-CUS-IDM-BIRTH-DATE         PIC X(10).             01180000
               05  DE-CUS-ADR-ADDR-TYPE          PIC X(2).              01190000
               05  DE-CUS-ADR-ADDR-LINE1         PIC X(64).             01200000
               05  DE-CUS-ADR-ADDR-LINE2         PIC X(64).             01210000
               05  DE-CUS-ADR-ADDR-LINE3         PIC X(64).             01220000
               05  DE-CUS-ADR-CITY               PIC X(30).             01230000
               05  DE-CUS-ADR-STATE-CODE         PIC X(2).              01240000
               05  DE-CUS-ADR-COUNTRY-CODE       PIC X(2).              01250000
               05  DE-CUS-ADR-ZIP-CODE           PIC 9(5).              01260000
               05  DE-CUS-ADR-ZIP-PLUS4          PIC 9(4).              01270000
               05  DE-CUS-ADR-PROVINCE-CODE      PIC X(2).              01280000
               05  DE-CUS-ADR-FOREIGN-ZIP-CODE   PIC X(10).             01290000
               05  DE-CUS-ADR-DELIVERY-PT-NO     PIC 9(2).              01300000
               05  DE-CUS-ADR-DELIVERY-PT-CHK    PIC 9(1).              01310000
               05  DE-CUS-ADR-CARRIER-ROUTE      PIC X(4).              01320000
               05  DE-CUS-ADR-EFF-DATE-IND       PIC X(1).              01330000
               05  DE-CUS-ADR-EFF-DATE           PIC X(10).             01340000
               05  DE-CUS-ADR-EXP-DATE-IND       PIC X(1).              01350000
               05  DE-CUS-ADR-EXP-DATE           PIC X(10).             01360000
               05  DE-CUS-ADR-ENT-TIMESTAMP      PIC X(26).             01370000
               05  DE-CUS-NME-NAME-TYPE          PIC X(1).              01380000
               05  DE-CUS-NME-NAME               PIC X(50).             01390000
               05  DE-CUS-NME-NAME1              PIC X(30).             01400000
               05  DE-CUS-NME-NAME2              PIC X(30).             01410000
               05  DE-CUS-NME-NAME3              PIC X(30).             01420000
               05  DE-CUS-NME-NAME4              PIC X(30).             01430000
               05  DE-CUS-NME-NAME-COMMON        PIC X(30).             01440000
               05  DE-CUS-NME-NAME-PREFIX        PIC X(30).             01450000
               05  DE-CUS-NME-NAME-SUFFIX        PIC X(30).             01460000
               05  DE-CUS-NME-EFF-DATE-IND       PIC X(1).              01470000
               05  DE-CUS-NME-EFF-DATE           PIC X(10).             01480000
               05  DE-CUS-NME-EXP-DATE-IND       PIC X(1).              01490000
               05  DE-CUS-NME-EXP-DATE           PIC X(10).             01500000
               05  DE-CUS-NME-ENT-TIMESTAMP      PIC X(26).             01510000
               05  DE-CUS-TIN-TIN-NO-FILL        PIC X(2).              01520000
               05  DE-CUS-TIN-TIN-NO             PIC 9(9).              01530000
               05  DE-CUS-TIN-TIN-TYPE           PIC X(1).              01540000
               05  DE-CUS-TIN-EFF-DATE-IND       PIC X(1).              01550000
               05  DE-CUS-TIN-EFF-DATE           PIC X(10).             01560000
               05  DE-CUS-TIN-EXP-DATE-IND       PIC X(1).              01570000
               05  DE-CUS-TIN-EXP-DATE           PIC X(10).             01580000
               05  DE-CUS-TIN-ENT-TIMESTAMP      PIC X(26).             01590000
               05  DE-CUS-XRF-XREF-TYPE          PIC X(8).              01600000
               05  DE-CUS-XRF-XREF-TEXT          PIC X(40).             01610000
               05  DE-CUS-CHANGE-WHO-CODE        PIC X(3).              01620000
               05  DE-CUS-CHANGE-DATE            PIC X(10).             01630000
               05  DE-CUS-CHANGE-TERM            PIC X(4).              01640000
               05  DE-CUS-ADR-USE-ZIP4-EDIT-SW   PIC X(1).              01650000
               05  DE-CUS-COOKIE-AREA            PIC X(100).            01660000
               05  DE-CUS-SUB-NO                 PIC 9(3).              01670000
               05  DE-CUS-CITIZENSHIP            PIC X(2).              01680000
               05  DE-CUS-IDM-GOVT-ID-TYPE       PIC X(2).              01690000
               05  DE-CUS-IDM-GOVT-ID            PIC X(25).             01700000
               05  DE-CUS-IDM-NASD-AFFIL         PIC X(1).              01710000
               05  DE-CUS-IDM-OFAC-NUMBER        PIC X(20).             01720000
               05  DE-CUS-IDM-FATF-NUMBER        PIC X(20).             01730000
050704         05  DE-CUS-OMIT-CUS-LOG-SW        PIC X(1).              01740000
               05  DE-CUS-IDM-EMPL-NAME          PIC X(30).             01750000
               05  DE-CUS-IDM-EMPL-ADDR-LINE1    PIC X(30).             01760000
               05  DE-CUS-IDM-EMPL-CITY          PIC X(30).             01770000
               05  DE-CUS-IDM-EMPL-STATE         PIC X(2).              01780000
               05  DE-CUS-IDM-EMPL-ZIP-CODE      PIC 9(5).              01790000
               05  DE-CUS-IDM-EMPL-ZIP-PLUS4     PIC 9(4).              01800000
               05  DE-CUS-IDM-EMPL-FOREIGN-ZIP   PIC X(10).             01810000
               05  DE-CUS-IDM-EMPL-COUNTRY-CODE  PIC X(2).              01820000
               05  DE-CUS-IDM-EMPL-PHONE-CNTRY   PIC X(5).              01830000
               05  DE-CUS-IDM-EMPL-PHN-CITY-CD   PIC X(5).              01840000
               05  DE-CUS-IDM-EMPL-PHONE-NO      PIC X(15).             01850000
               05  DE-CUS-IDM-EMPL-FOREIGN-SW    PIC X(1).              01860000
               05  DE-CUS-IDM-EMPL-BUS-NATURE    PIC X(30).             01870000
               05  DE-CUS-IDM-OCCUPATION         PIC X(30).             01880000
               05  DE-CUS-IDM-YEARS-EMPLOYED     PIC 9(2).              01890000
               05  DE-CUS-IDM-SRC-OF-FUNDS       PIC X(1).              01900000
               05  DE-CUS-IDM-BRR-LETTER-DATE    PIC X(10).             01910000
               05  DE-CUS-IDM-EMPL-ZIP4-SW       PIC X(1).              01920000
               05  DE-CUS-IDM-EMPL-ADDR-LINE2    PIC X(30).             01930000
               05  DE-CUS-IDM-RISK-REVIEW-IND    PIC X(2).              01940000
070203         05  DE-CUS-BRANCH                 PIC X(4).              01950000
061903         05  DE-CUS-IDM-EST-BDATE-IND      PIC X(1).              01960000
100203         05  DE-CUS-IDM-ISSUANCE-PLACE     PIC X(2).              01970000
100203         05  DE-CUS-IDM-ISSUE-DATE         PIC X(10).             01980000
100203         05  DE-CUS-IDM-EXPIRATION-DATE    PIC X(10).             01990000
100203         05  DE-CUS-IDM-SAR-DATE           PIC X(10).             02000000
010804         05  DE-CUS-SUP-CDC-MIS            PIC X.                 02010000
010804         05  DE-CUS-SUP-BRR-LTR            PIC X.                 02020000
040704         05  DE-CUS-IDM-PERSON-OF-INT      PIC X(2).              02030000
040704         05  DE-CUS-IDM-EMPL-PROVINCE      PIC X(2).              02040000
040704         05  DE-CUS-IDM-MET-IN-PERSON      PIC X(1).              02050000
061704         05  DE-CUS-CUSTOMER-TYPE          PIC X(4).              02060000
120706         05  DE-CUS-IDM-OFAC-IND           PIC X(1).              02070000
120706         05  DE-CUS-IDM-OFAC-CHG-TIMESTAMP PIC X(26).             02080000
120706         05  DE-CUS-IDM-OFAC-CHG-WHO-CODE  PIC X(3).              02090000
011208         05  DE-CUS-IDM-EMAIL-ADDRESS      PIC X(70).             02100000
120408         05  DE-CUS-ADD-CUSTOMER-ID        PIC 9(18).             02110000
120408         05  FILLER REDEFINES DE-CUS-ADD-CUSTOMER-ID.             02120000
120408             06  DE-CUS-ADD-CUSTOMER-ID-1  PIC 9(6).              02130000
120408             06  DE-CUS-ADD-CUSTOMER-ID-2  PIC 9(12).             02140000
031514         05  DE-CUS-BDM-BUS-TYPE           PIC X(1).              02150000
031514         05  DE-CUS-BDM-BUS-SUB-TYPE       PIC X(2).              02160000
031514         05  DE-CUS-BDM-PUB-TRAD           PIC X(1).              02170000
031514         05  DE-CUS-BDM-EXCH               PIC X(7).              02180000
031514         05  DE-CUS-BDM-EXCH-CTRY          PIC X(2).              02190000
031514         05  DE-CUS-BDM-EXCH-DESC          PIC X(40).             02200000
031514         05  DE-CUS-BDM-NAICS1             PIC X(6).              02210000
031514         05  DE-CUS-BDM-NAICS2             PIC X(6).              02220000
031514         05  DE-CUS-BDM-NAICS3             PIC X(6).              02230000
031514         05  DE-CUS-BDM-CTRY-REG           PIC X(2).              02240000
031514         05  DE-CUS-BDM-ST-REG             PIC X(2).              02250000
031514         05  DE-CUS-BDM-SALES-MKT          PIC X(4).              02260000
031514         05  DE-CUS-BDM-P-MRKT-CTRY1       PIC X(2).              02270000
031514         05  DE-CUS-BDM-P-MRKT-CTRY2       PIC X(2).              02280000
031514         05  DE-CUS-BDM-P-MRKT-CTRY3       PIC X(2).              02290000
031514         05  DE-CUS-BDM-PRI-MKT-STATES1    PIC X(2).              02300000
031514         05  DE-CUS-BDM-PRI-MKT-STATES2    PIC X(2).              02310000
031514         05  DE-CUS-BDM-PRI-MKT-STATES3    PIC X(2).              02320000
031514         05  DE-CUS-BDM-PCT-OWNSHP         PIC 9(3).              02330000
031514         05  DE-CUS-BDM-PERM-RES           PIC X(1).              02340000
031514         05  DE-CUS-BDM-NPRFT              PIC X(1).              02350000
031514         05  DE-CUS-BDM-DECD               PIC X(1).              02360000
031514         05  DE-CUS-BDM-CTRY-EXCP          PIC X(2).              02370000
031514         05  DE-CUS-BDM-CTRY-EXCP-DT       PIC X(10).             02380000
031514         05  DE-CUS-BDM-CHANGE-SW          PIC X(1).              02390000
031A14         05  DE-CUS-REL-TYPE               PIC X(08).             02400000
011616         05  DE-CUS-SECONDARY-TYPE         PIC X(4).              02410000
011616         05  DE-CUS-COE-SW                 PIC X(1).              02420000
071616         05  DE-CUS-NME-NAME-EXP           PIC X(150).            02430000
071616         05  FILLER                        PIC X(1211).           02440000
               05  DE-CUS-PHN-LAYOUT.                                   02450000
                   06  DE-CUS-HOME-CNTRY-CODE    PIC X(5).              02460000
                   06  DE-CUS-BUS-CNTRY-CODE     PIC X(5).              02470000
                   06  DE-CUS-FAX-CNTRY-CODE     PIC X(5).              02480000
                   06  DE-CUS-OTHER-CNTRY-CODE   PIC X(5).              02490000
                   06  DE-CUS-CELL-CNTRY-CODE    PIC X(5).              02500000
                   06  DE-CUS-HOME-CITY-CODE     PIC X(5).              02510000
                   06  DE-CUS-BUS-CITY-CODE      PIC X(5).              02520000
                   06  DE-CUS-FAX-CITY-CODE      PIC X(5).              02530000
                   06  DE-CUS-OTHER-CITY-CODE    PIC X(5).              02540000
                   06  DE-CUS-CELL-CITY-CODE     PIC X(5).              02550000
                   06  DE-CUS-PHONE-HOME         PIC X(15).             02560000
                   06  DE-CUS-PHONE-BUSINESS     PIC X(15).             02570000
                   06  DE-CUS-PHONE-FAX          PIC X(15).             02580000
                   06  DE-CUS-PHONE-CELL         PIC X(15).             02590000
                   06  DE-CUS-PHONE-OTHER        PIC X(15).             02600000
                   06  DE-CUS-BUSINESS-EXT       PIC X(10).             02610000
                   06  DE-CUS-HOME-FOREIGN-SW    PIC X(1).              02620000
                   06  DE-CUS-BUS-FOREIGN-SW     PIC X(1).              02630000
                   06  DE-CUS-FAX-FOREIGN-SW     PIC X(1).              02640000
                   06  DE-CUS-OTHER-FOREIGN-SW   PIC X(1).              02650000
                   06  DE-CUS-CELL-FOREIGN-SW    PIC X(1).              02660000
                   06  FILLER                    PIC X(400).            02670000
111409         05  DE-CUS-IDM-PERSON-OF-INT2     PIC X(2).              02680000
111409         05  DE-CUS-IDM-PERSON-OF-INT3     PIC X(2).              02690000
111409         05  DE-CUS-IDM-PERSON-OF-INT4     PIC X(2).              02700000
111409         05  DE-CUS-IDM-PERSON-OF-INT5     PIC X(2).              02710000
031514         05  DE-CUS-IDM-OCCUPATION-DESC    PIC X(28).             02720000
101114         05  FILLER                        PIC X(27).             02730000
    *******************************************************             02740000
    ***  THE FOLLOWING FIELDS ARE USED TO PASS BACK AND  **             02750000
    ***  FORTH BETWEEN THE ENGINES AND ARE NOT USED      **             02760000
    ***  AS INQUIRY OR UPDATE.  WHEN ADDING FIELDS TO    **             02770000
    ***  THIS AREA, ADD THEM ABOVE THE FIRST FIELD SO AS **             02780000
    ***  TO NOT COMPROMISE THE OFFSETS OF THE OTHER      **             02790000
    ***  PASS THRU FIELDS.                               **             02800000
    *******************************************************             02810000
101114         05  DE-CUS-NAME-TYPE-EDIT         PIC X(1).              02820000
111409         05  DE-CUS-PERSON-OF-INT-IND      PIC X(1).              02830000
111409         05  DE-CUS-PERSON-OF-INT2-IND     PIC X(1).              02840000
111409         05  DE-CUS-PERSON-OF-INT3-IND     PIC X(1).              02850000
111409         05  DE-CUS-PERSON-OF-INT4-IND     PIC X(1).              02860000
111409         05  DE-CUS-PERSON-OF-INT5-IND     PIC X(1).              02870000
120408         05  DE-CUS-HOME-PHONE-IND         PIC X(1).              02880000
120408         05  DE-CUS-BUS-PHONE-IND          PIC X(1).              02890000
120408         05  DE-CUS-OTHER-PHONE-IND        PIC X(1).              02900000
120408         05  DE-CUS-FAX-PHONE-IND          PIC X(1).              02910000
120408         05  DE-CUS-ANY-PHONE-IND          PIC X(1).              02920000
120408         05  DE-CUS-DOB-IND                PIC X(1).              02930000
120408         05  DE-CUS-OCCUPATION-IND         PIC X(1).              02940000
120408         05  DE-CUS-NASD-AFFILIATN-IND     PIC X(1).              02950000
120408         05  DE-CUS-ISSUANCE-PLACE-IND     PIC X(1).              02960000
120408         05  DE-CUS-ISSUE-DATE-IND         PIC X(1).              02970000
120408         05  DE-CUS-EXPIRATION-DTE-IND     PIC X(1).              02980000
120408         05  DE-CUS-BUS-NATURE-IND         PIC X(1).              02990000
120408         05  DE-CUS-TAXID-IND              PIC X(1).              03000000
120408         05  DE-CUS-MIXED-CASE-SW          PIC X(1).              03010000
060605         05  DE-CUS-CUST-TYPE-REF          PIC X(4).              03020000
               05  DE-CUS-BRR-LTR-LOG            PIC X(1).              03030000
               05  DE-CUS-CR-CHECK-LOG           PIC X(1).              03040000
               05  DE-CUS-FIRM-USES-CUST-ID-SW   PIC X(1).              03050000
               05  DE-CUS-AUTO-UPDATE-REPS-SW    PIC X(1).              03060000
               05  DE-CUS-AUTO-UPDATE-ADDR-SW    PIC X(1).              03070000
               05  DE-CUS-KEY-FOUND-SW           PIC X(1).              03080000
               05  DE-CUS-PHN-CHANGE-SW          PIC X(1).              03090000
               05  DE-CUS-NME-CHANGE-SW          PIC X(1).              03100000
               05  DE-CUS-ADR-CHANGE-SW          PIC X(1).              03110000
               05  DE-CUS-TIN-CHANGE-SW          PIC X(1).              03120000
               05  DE-CUS-IDM-CHANGE-SW          PIC X(1).              03130000
               05  DE-CUS-XRF-CHANGE-SW          PIC X(1).              03140000
               05  DE-CUS-REP-CHANGE-SW          PIC X(1).              03150000
               05  DE-CUS-KEY-EDITTED-SW         PIC X(1).              03160000
           04  DE-CUSTOMER-LAYOUT-X REDEFINES DE-CUSTOMER-LAYOUT.       03170000
               05  DE-CUS-CUSTOMER-ID-X          PIC X(18).             03180000
               05  DE-CUS-ACCT-NO-X              PIC X(8).              03190000
               05  DE-CUS-REP-REP-NO-X           PIC X(4).              03200000
               05  DE-CUS-DATE-IND-X             PIC X(1).              03210000
               05  DE-CUS-IDM-BIRTH-DATE-X       PIC X(10).             03220000
               05  DE-CUS-ADR-ADDR-TYPE-X        PIC X(2).              03230000
               05  DE-CUS-ADR-ADDR-LINE1-X       PIC X(64).             03240000
               05  DE-CUS-ADR-ADDR-LINE2-X       PIC X(64).             03250000
               05  DE-CUS-ADR-ADDR-LINE3-X       PIC X(64).             03260000
               05  DE-CUS-ADR-CITY-X             PIC X(30).             03270000
               05  DE-CUS-ADR-STATE-CODE-X       PIC X(2).              03280000
               05  DE-CUS-ADR-COUNTRY-CODE-X     PIC X(2).              03290000
               05  DE-CUS-ADR-ZIP-CODE-X         PIC X(5).              03300000
               05  DE-CUS-ADR-ZIP-PLUS4-X        PIC X(4).              03310000
               05  DE-CUS-ADR-PROVINCE-CODE-X    PIC X(2).              03320000
               05  DE-CUS-ADR-FOREIGN-ZIP-CODE-X PIC X(10).             03330000
               05  DE-CUS-ADR-DELIVERY-PT-NO-X   PIC X(2).              03340000
               05  DE-CUS-ADR-DELIVERY-PT-CHK-X  PIC X(1).              03350000
               05  DE-CUS-ADR-CARRIER-ROUTE-X    PIC X(4).              03360000
               05  DE-CUS-ADR-EFF-DATE-IND-X     PIC X(1).              03370000
               05  DE-CUS-ADR-EFF-DATE-X         PIC X(10).             03380000
               05  DE-CUS-ADR-EXP-DATE-IND-X     PIC X(1).              03390000
               05  DE-CUS-ADR-EXP-DATE-X         PIC X(10).             03400000
               05  DE-CUS-ADR-ENT-TIMESTAMP-X    PIC X(26).             03410000
               05  DE-CUS-NME-NAME-TYPE-X        PIC X(1).              03420000
               05  DE-CUS-NME-NAME-X             PIC X(50).             03430000
               05  DE-CUS-NME-NAME1-X            PIC X(30).             03440000
               05  DE-CUS-NME-NAME2-X            PIC X(30).             03450000
               05  DE-CUS-NME-NAME3-X            PIC X(30).             03460000
               05  DE-CUS-NME-NAME4-X            PIC X(30).             03470000
               05  DE-CUS-NME-NAME-COMMON-X      PIC X(30).             03480000
               05  DE-CUS-NME-NAME-PREFIX-X      PIC X(30).             03490000
               05  DE-CUS-NME-NAME-SUFFIX-X      PIC X(30).             03500000
               05  DE-CUS-NME-EFF-DATE-IND-X     PIC X(1).              03510000
               05  DE-CUS-NME-EFF-DATE-X         PIC X(10).             03520000
               05  DE-CUS-NME-EXP-DATE-IND-X     PIC X(1).              03530000
               05  DE-CUS-NME-EXP-DATE-X         PIC X(10).             03540000
               05  DE-CUS-NME-ENT-TIMESTAMP-X    PIC X(26).             03550000
               05  DE-CUS-TIN-TIN-NO-X           PIC X(11).             03560000
               05  DE-CUS-TIN-TIN-TYPE-X         PIC X(1).              03570000
               05  DE-CUS-TIN-EFF-DATE-IND-X     PIC X(1).              03580000
               05  DE-CUS-TIN-EFF-DATE-X         PIC X(10).             03590000
               05  DE-CUS-TIN-EXP-DATE-IND-X     PIC X(1).              03600000
               05  DE-CUS-TIN-EXP-DATE-X         PIC X(10).             03610000
               05  DE-CUS-TIN-ENT-TIMESTAMP-X    PIC X(26).             03620000
               05  DE-CUS-XRF-XREF-TYPE-X        PIC X(8).              03630000
               05  DE-CUS-XRF-XREF-TEXT-X        PIC X(40).             03640000
               05  DE-CUS-CHANGE-WHO-CODE-X      PIC X(3).              03650000
               05  DE-CUS-CHANGE-DATE-X          PIC X(10).             03660000
               05  DE-CUS-CHANGE-TERM-X          PIC X(4).              03670000
               05  DE-CUS-ADR-USE-ZIP4-EDIT-SW-X PIC X(1).              03680000
               05  DE-CUS-COOKIE-AREA-X          PIC X(100).            03690000
               05  DE-CUS-SUB-NO-X               PIC X(3).              03700000
               05  DE-CUS-CITIZENSHIP-X          PIC X(2).              03710000
               05  DE-CUS-IDM-GOVT-ID-TYPE-X     PIC X(2).              03720000
               05  DE-CUS-IDM-GOVT-ID-X          PIC X(25).             03730000
               05  DE-CUS-IDM-NASD-AFFIL-X       PIC X(1).              03740000
               05  DE-CUS-IDM-OFAC-NUMBER-X      PIC X(20).             03750000
               05  DE-CUS-IDM-FATF-NUMBER-X      PIC X(20).             03760000
050704         05  DE-CUS-OMIT-CUS-LOG-SW-X      PIC X(1).              03770000
               05  DE-CUS-IDM-EMPL-NAME-X        PIC X(30).             03780000
               05  DE-CUS-IDM-EMPL-ADDR-LINE1-X  PIC X(30).             03790000
               05  DE-CUS-IDM-EMPL-CITY-X        PIC X(30).             03800000
               05  DE-CUS-IDM-EMPL-STATE-X       PIC X(2).              03810000
               05  DE-CUS-IDM-EMPL-ZIP-CODE-X    PIC X(5).              03820000
               05  DE-CUS-IDM-EMPL-ZIP-PLUS4-X   PIC X(4).              03830000
               05  DE-CUS-IDM-EMPL-FOREIGN-ZIP-X PIC X(10).             03840000
               05  DE-CUS-IDM-EMPL-COUNTRY-CD-X  PIC X(2).              03850000
               05  DE-CUS-IDM-EMPL-PHONE-CNTRY-X PIC X(5).              03860000
               05  DE-CUS-IDM-EMPL-PHN-CITY-CD-X PIC X(5).              03870000
               05  DE-CUS-IDM-EMPL-PHONE-NO-X    PIC X(15).             03880000
               05  DE-CUS-IDM-EMPL-FOREIGN-SW-X  PIC X(1).              03890000
               05  DE-CUS-IDM-EMPL-BUS-NATURE-X  PIC X(30).             03900000
               05  DE-CUS-IDM-OCCUPATION-X       PIC X(30).             03910000
               05  DE-CUS-IDM-YEARS-EMPLOYED-X   PIC X(2).              03920000
               05  DE-CUS-IDM-SRC-OF-FUNDS-X     PIC X(1).              03930000
               05  DE-CUS-IDM-BRR-LETTER-DATE-X  PIC X(10).             03940000
               05  DE-CUS-IDM-EMPL-ZIP4-SW-X     PIC X(1).              03950000
               05  DE-CUS-IDM-EMPL-ADDR-LINE2-X  PIC X(30).             03960000
               05  DE-CUS-IDM-RISK-REVIEW-IND-X  PIC X(2).              03970000
070203         05  DE-CUS-BRANCH-X               PIC X(4).              03980000
061903         05  DE-CUS-IDM-EST-BDATE-IND-X    PIC X(1).              03990000
100203         05  DE-CUS-IDM-ISSUANCE-PLACE-X   PIC X(2).              04000000
100203         05  DE-CUS-IDM-ISSUE-DATE-X       PIC X(10).             04010000
100203         05  DE-CUS-IDM-EXPIRATION-DATE-X  PIC X(10).             04020000
100203         05  DE-CUS-IDM-SAR-DATE-X         PIC X(10).             04030000
010804         05  DE-CUS-SUP-CDC-MIS-X          PIC X.                 04040000
010804         05  DE-CUS-SUP-BRR-LTR-X          PIC X.                 04050000
040704         05  DE-CUS-IDM-PERSON-OF-INT-X    PIC X(2).              04060000
040704         05  DE-CUS-IDM-EMPL-PROVINCE-X    PIC X(2).              04070000
040704         05  DE-CUS-IDM-MET-IN-PERSON-X    PIC X(1).              04080000
061704         05  DE-CUS-CUSTOMER-TYPE-X        PIC X(4).              04090000
120706         05  DE-CUS-IDM-OFAC-IND-X         PIC X(1).              04100000
120706         05  DE-CUS-IDM-OFAC-CHG-TIMESTMP-X                       04110000
120706                                           PIC X(26).             04120000
120706         05  DE-CUS-IDM-OFAC-CHG-WHO-CODE-X                       04130000
120706                                           PIC X(3).              04140000
011208         05  DE-CUS-IDM-EMAIL-ADDRESS-X    PIC X(70).             04150000
120408         05  DE-CUS-ADD-CUSTOMER-ID-X      PIC X(18).             04160000
031514         05  DE-CUS-BDM-BUS-TYPE-X         PIC X(1).              04170000
031514         05  DE-CUS-BDM-BUS-SUB-TYPE-X     PIC X(2).              04180000
031514         05  DE-CUS-BDM-PUB-TRAD-X         PIC X(1).              04190000
031514         05  DE-CUS-BDM-EXCH-X             PIC X(7).              04200000
031514         05  DE-CUS-BDM-EXCH-CTRY-X        PIC X(2).              04210000
031514         05  DE-CUS-BDM-EXCH-DESC-X        PIC X(40).             04220000
031514         05  DE-CUS-BDM-NAICS1-X           PIC X(6).              04230000
031514         05  DE-CUS-BDM-NAICS2-X           PIC X(6).              04240000
031514         05  DE-CUS-BDM-NAICS3-X           PIC X(6).              04250000
031514         05  DE-CUS-BDM-CTRY-REG-X         PIC X(2).              04260000
031514         05  DE-CUS-BDM-ST-REG-X           PIC X(2).              04270000
031514         05  DE-CUS-BDM-SALES-MKT-X        PIC X(4).              04280000
031514         05  DE-CUS-BDM-P-MRKT-CTRY1-X     PIC X(2).              04290000
031514         05  DE-CUS-BDM-P-MRKT-CTRY2-X     PIC X(2).              04300000
031514         05  DE-CUS-BDM-P-MRKT-CTRY3-X     PIC X(2).              04310000
031514         05  DE-CUS-BDM-PRI-MKT-STATES1-X  PIC X(2).              04320000
031514         05  DE-CUS-BDM-PRI-MKT-STATES2-X  PIC X(2).              04330000
031514         05  DE-CUS-BDM-PRI-MKT-STATES3-X  PIC X(2).              04340000
031514         05  DE-CUS-BDM-PCT-OWNSHP-X       PIC X(3).              04350000
031514         05  DE-CUS-BDM-PERM-RES-X         PIC X(1).              04360000
031514         05  DE-CUS-BDM-NPRFT-X            PIC X(1).              04370000
031514         05  DE-CUS-BDM-DECD-X             PIC X(1).              04380000
031514         05  DE-CUS-BDM-CTRY-EXCP-X        PIC X(2).              04390000
031514         05  DE-CUS-BDM-CTRY-EXCP-DT-X     PIC X(10).             04400000
031514         05  DE-CUS-BDM-CHANGE-SW-X        PIC X(1).              04410000
031A14         05  DE-CUS-REL-TYPE-X             PIC X(08).             04420000
011616         05  DE-CUS-SECONDARY-TYPE-X       PIC X(4).              04430000
011616         05  DE-CUS-COE-SW-X               PIC X(1).              04440000
071616         05  DE-CUS-NME-NAME-EXP-X         PIC X(150).            04450000
071616         05  FILLER                        PIC X(1211).           04460000
               05  DE-CUS-PHN-LAYOUT-X.                                 04470000
                   06  DE-CUS-HOME-CNTRY-CODE-X  PIC X(5).              04480000
                   06  DE-CUS-BUS-CNTRY-CODE-X   PIC X(5).              04490000
                   06  DE-CUS-FAX-CNTRY-CODE-X   PIC X(5).              04500000
                   06  DE-CUS-OTHER-CNTRY-CODE-X PIC X(5).              04510000
                   06  DE-CUS-CELL-CNTRY-CODE-X  PIC X(5).              04520000
                   06  DE-CUS-HOME-CITY-CODE-X   PIC X(5).              04530000
                   06  DE-CUS-BUS-CITY-CODE-X    PIC X(5).              04540000
                   06  DE-CUS-FAX-CITY-CODE-X    PIC X(5).              04550000
                   06  DE-CUS-OTHER-CITY-CODE-X  PIC X(5).              04560000
                   06  DE-CUS-CELL-CITY-CODE-X   PIC X(5).              04570000
                   06  DE-CUS-PHONE-HOME-X       PIC X(15).             04580000
                   06  DE-CUS-PHONE-BUSINESS-X   PIC X(15).             04590000
                   06  DE-CUS-PHONE-FAX-X        PIC X(15).             04600000
                   06  DE-CUS-PHONE-CELL-X       PIC X(15).             04610000
                   06  DE-CUS-PHONE-OTHER-X      PIC X(15).             04620000
                   06  DE-CUS-BUSINESS-EXT-X     PIC X(10).             04630000
                   06  DE-CUS-HOME-FOREIGN-SW-X  PIC X(1).              04640000
                   06  DE-CUS-BUS-FOREIGN-SW-X   PIC X(1).              04650000
                   06  DE-CUS-FAX-FOREIGN-SW-X   PIC X(1).              04660000
                   06  DE-CUS-OTHER-FOREIGN-SW-X PIC X(1).              04670000
                   06  DE-CUS-CELL-FOREIGN-SW-X  PIC X(1).              04680000
                   06  FILLER                    PIC X(400).            04690000
111409         05  DE-CUS-IDM-PERSON-OF-INT2-X   PIC X(2).              04700000
111409         05  DE-CUS-IDM-PERSON-OF-INT3-X   PIC X(2).              04710000
111409         05  DE-CUS-IDM-PERSON-OF-INT4-X   PIC X(2).              04720000
111409         05  DE-CUS-IDM-PERSON-OF-INT5-X   PIC X(2).              04730000
031514         05  DE-CUS-IDM-OCCUPATION-DESC-X  PIC X(28).             04740000
101114         05  FILLER                        PIC X(27).             04750000
101114         05  DE-CUS-NAME-TYPE-EDIT-X       PIC X(1).              04760000
101114         05  FILLER                        PIC X(5).              04770000
120408         05  DE-CUS-HOME-PHONE-IND-X       PIC X(1).              04780000
120408         05  DE-CUS-BUS-PHONE-IND-X        PIC X(1).              04790000
120408         05  DE-CUS-OTHER-PHONE-IND-X      PIC X(1).              04800000
120408         05  DE-CUS-FAX-PHONE-IND-X        PIC X(1).              04810000
120408         05  DE-CUS-ANY-PHONE-IND-X        PIC X(1).              04820000
120408         05  DE-CUS-DOB-IND-X              PIC X(1).              04830000
120408         05  DE-CUS-OCCUPATION-IND-X       PIC X(1).              04840000
120408         05  DE-CUS-NASD-AFFILIATN-IND-X   PIC X(1).              04850000
120408         05  DE-CUS-ISSUANCE-PLACE-IND-X   PIC X(1).              04860000
120408         05  DE-CUS-ISSUE-DATE-IND-X       PIC X(1).              04870000
120408         05  DE-CUS-EXPIRATION-DTE-IND-X   PIC X(1).              04880000
120408         05  DE-CUS-BUS-NATURE-IND-X       PIC X(1).              04890000
120408         05  DE-CUS-TAXID-IND-X            PIC X(1).              04900000
120408         05  DE-CUS-MIXED-CASE-SW-X        PIC X(1).              04910000
060605         05  DE-CUS-CUST-TYPE-REF-X        PIC X(4).              04920000
050704         05  DE-CUS-BRR-LTR-LOG-X          PIC X(1).              04930000
050704         05  DE-CUS-CR-CHECK-LOG-X         PIC X(1).              04940000
               05  DE-CUS-FIRM-USES-CUST-ID-SW-X PIC X(1).              04950000
               05  DE-CUS-AUTO-UPDATE-REPS-SW-X  PIC X(1).              04960000
               05  DE-CUS-AUTO-UPDATE-ADDR-SW-X  PIC X(1).              04970000
               05  DE-CUS-KEY-FOUND-SW-X         PIC X(1).              04980000
               05  DE-CUS-PHN-CHANGE-SW-X        PIC X(1).              04990000
               05  DE-CUS-NME-CHANGE-SW-X        PIC X(1).              05000000
               05  DE-CUS-ADR-CHANGE-SW-X        PIC X(1).              05010000
               05  DE-CUS-TIN-CHANGE-SW-X        PIC X(1).              05020000
               05  DE-CUS-IDM-CHANGE-SW-X        PIC X(1).              05030000
               05  DE-CUS-XRF-CHANGE-SW-X        PIC X(1).              05040000
               05  DE-CUS-REP-CHANGE-SW-X        PIC X(1).              05050000
               05  DE-CUS-KEY-EDITTED-SW-X       PIC X(1).              05060000
                                                                        05070000
*******  DATA ENGINE RETURN CODES                                       05080000
           04  DE-RETURN-CODE-TABLE.                                    05090000
               05  DE-RETURN-CODES                    OCCURS 50 TIMES.  05100000
                   10  DE-RETURN-MSG-IND              PIC X(1).         05110000
                       88  DE-RETURN-WARNING          VALUE 'W'.        05120000
                       88  DE-RETURN-EDIT-ERROR       VALUE 'E'.        05130000
                       88  DE-RETURN-SYSTEM-ERROR     VALUE 'S'.        05140000
                   10  DE-RETURN-CODE                 PIC 9(4).         05150000
WEB            05  FILLER                             PIC X(50).        05160002
