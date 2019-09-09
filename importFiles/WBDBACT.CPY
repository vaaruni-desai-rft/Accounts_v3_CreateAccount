      **************************************************************    00010000
ZX    * PAUL FAIT                                                       00020000
ZX    * ZX/ZX/ZX HOST0????? WR39325                                     00030000
ZX    *          NEW WEB COPYBOOK FOR IIB (EO220801)                    00040000
ZX    *          DBCOMM + DBACT                                         00050000
      **************************************************************    00060000
WEB    01  WEB-DBACT-PARAMETERS.                                        00070000
                                                                        00070100
WEB   *          DBCOMM                                                 00071000
           03  DB-CM-REQUEST-FIELDS.                                    00080000
               04  DB-CM-ENGINE-ID               PIC X(8).              00090000
               04  DB-CM-VIEW-ID                 PIC X(2).              00100000
               04  DB-CM-KEY-ID                  PIC X(2).              00110000
               04  DB-CM-REQUEST-IND             PIC X(1).              00120000
                   88  DB-CM-REQ-OPEN                VALUE 'O'.         00130000
                   88  DB-CM-REQ-CLOSE               VALUE 'C'.         00140000
                   88  DB-CM-REQ-RETRIEVE            VALUE 'R'.         00150000
                   88  DB-CM-REQ-INSERT              VALUE 'I'.         00160000
                   88  DB-CM-REQ-UPDATE              VALUE 'U'.         00170000
                   88  DB-CM-REQ-DELETE              VALUE 'D'.         00180000
               04  DB-CM-DIRECTION-IND           PIC X(1).              00190000
                   88  DB-CM-DIR-ASCENDING           VALUE 'A' SPACES.  00200000
                   88  DB-CM-DIR-DESCENDING          VALUE 'D'.         00210000
041214         04  DB-CM-ORDER-ID                REDEFINES              00220000
041214             DB-CM-DIRECTION-IND           PIC X(1).              00230000
               04  DB-CM-LOCK-IND                PIC X(1).              00240000
                   88  DB-CM-LOCK                    VALUE 'Y'.         00250000
               04  DB-CM-ORIGIN-ID               PIC X(8).              00260000
041809         04  DB-CM-RETRIEVE-NBR            PIC S9(7) COMP-3.      00270000
050913         04  DC-CM-PERSIST-CSR-IND         PIC X(1).              00280000
050913             88  DB-CM-PERSIST-CSR             VALUE 'Y'.         00290000
011715         04  DB-PARM1-LEN                  PIC S9(5) COMP-3.      00300000
011715         04  DB-PARM2-LEN                  PIC S9(5) COMP-3.      00310000
011715         04  DB-PARM3-LEN                  PIC S9(5) COMP-3.      00320000
011715         04  DB-PARM4-LEN                  PIC S9(5) COMP-3.      00330000
011715         04  FILLER                        PIC X(36).             00340000
                                                                        00350000
           03  DB-CM-RESULT-FIELDS.                                     00360000
               04  DB-CM-RETURN-APP-CODE         PIC X(8).              00370000
               04  DB-CM-RETURN-CODE             PIC X(5).              00380000
                   88  DB-CM-OK                      VALUE  SPACES.     00390000
                   88  DB-CM-INVALID-VIEW-ID         VALUE '00001'.     00400000
                   88  DB-CM-INVALID-KEY-ID          VALUE '00002'.     00410000
                   88  DB-CM-INVALID-REQUEST-IND     VALUE '00003'.     00420000
                   88  DB-CM-INVALID-DATA            VALUE '00004'.     00430000
                   88  DB-CM-NOT-FOUND               VALUE '00014'.     00440000
101312             88  DB-CM-ALREADY-EXISTS          VALUE '00020'.     00450000
                   88  DB-CM-DB2-ERROR               VALUE '00092'      00460000
101312                                                     '00020'      00470000
                                                           '00014'.     00480000
               04  DB-CM-DBMS-IND                PIC X(1).              00490000
                   88  DB-CM-DB2                     VALUE 'D'.         00500000
               04  DB-CM-RETURN-MSG-IND          PIC X(1).              00510000
                   88  DB-CM-INFORMATIONAL           VALUE 'I'.         00520000
                   88  DB-CM-WARNING                 VALUE 'W'.         00530000
                   88  DB-CM-SEVERE-ERROR            VALUE 'S'.         00540000
               04  DB-CM-RETURN-AREA             PIC X(150).            00550000
               04  FILLER                        PIC X(59).             00560000
                                                                        00561000
WEB   *          DBACT                                                  00570000
           03  DB-ACT-KEY-FIELDS.                                       00580000
               04  DB-ACT-KEY-FIRM-NO         PIC S9(3)   COMP-3.       00590000
               04  DB-ACT-KEY-SUB-NO          PIC S9(3)   COMP-3.       00600000
               04  DB-ACT-KEY-ACCT-NO         PIC S9(9)   COMP-3.       00610000
               04  DB-ACT-KEY-BRS-BRANCH      PIC X(4).                 00620000
               04  DB-ACT-KEY-REP             PIC X(4).                 00630000
081311         04  DB-ACT-KEY-ALPHA-KEY-1     PIC X(30).                00640000
030713         04  DB-ACT-KEY-ALPHA-KEY       REDEFINES                 00650000
030713                                        DB-ACT-KEY-ALPHA-KEY-1    00660000
030713                                        PIC X(30).                00670000
081311         04  FILLER                     PIC X(120).               00680000
               04  DB-ACT-KEY-TAXID           PIC S9(9)   COMP-3.       00690000
100910         04  FILLER                     PIC X(3).                 00700000
100910         04  DB-ACT-KEY-MBSCC           PIC X(5).                 00710000
               04  DB-ACT-KEY-NINES-COMP-NO   PIC S9(9)   COMP-3.       00720000
               04  DB-ACT-KEY-PORTFOLIO-PRINT PIC X(2).                 00730000
               04  DB-ACT-KEY-MANAGED-ACCT-NO PIC S9(3)   COMP-3.       00740000
               04  DB-ACT-KEY-NASDQ-SYMBOL    PIC X(5).                 00750000
               04  DB-ACT-KEY-BROKER-NO       PIC S9(5)   COMP-3.       00760000
               04  DB-ACT-KEY-OTHER-DEP       PIC X(6).                 00770000
               04  DB-ACT-KEY-CMTA-NO         PIC S9(5)   COMP-3.       00780000
               04  DB-ACT-KEY-GSCC-NO         PIC S9(5)   COMP-3.       00790000
100110         04  FILLER                     PIC X(3).                 00800000
101009         04  DB-ACT-KEY-NCC-NO          PIC S9(5)   COMP-3.       00810000
               04  DB-ACT-KEY-MASTER-ACCT     PIC X(1).                 00820000
011208         04  DB-ACT-KEY-CONV-ACCT.                                00830000
011208             05 DB-ACT-KEY-CONV-FIRM-NO PIC 9(3).                 00840000
011208             05 DB-ACT-KEY-CONV-ACCT-NO PIC X(12).                00850000
011208         04  FILLER                     PIC X(35).                00860000
           03  DB-ACT-DATA-FIELDS.                                      00870000
               04  DB-ACT-FIRM-NO             PIC S9(3)   COMP-3.       00880000
               04  DB-ACT-ACCT-NO             PIC S9(9)   COMP-3.       00890000
               04  DB-ACT-SUB-NO              PIC S9(3)   COMP-3.       00900000
               04  DB-ACT-NINES-COMP-NO       PIC S9(9)   COMP-3.       00910000
               04  DB-ACT-REP                 PIC X(4).                 00920000
               04  DB-ACT-REP-PREVIOUS        PIC X(4).                 00930000
               04  DB-ACT-REP-B4-ACAT         PIC X(4).                 00940000
               04  DB-ACT-BRANCH              PIC X(4).                 00950000
               04  DB-ACT-BRANCH-PREVIOUS     PIC X(4).                 00960000
               04  DB-ACT-BRANCH-B4-ACAT      PIC X(4).                 00970000
               04  DB-ACT-GROUP-NO            PIC S9(9)   COMP-3.       00980000
               04  DB-ACT-CONV-ACCT           PIC X(15).                00990000
               04  DB-ACT-COMPANY-MERGE-CODE  PIC X(2).                 01000000
               04  DB-ACT-PURGE-IND           PIC X(1).                 01010000
               04  DB-ACT-NAMELINES           PIC S9(1).                01020000
               04  DB-ACT-IRS-NAMELINE1       PIC S9(1).                01030000
               04  DB-ACT-IRS-NAMELINE2       PIC S9(1).                01040000
               04  DB-ACT-NAME-ADDR-1         PIC X(64).                01050000
               04  DB-ACT-NAME-ADDR-2         PIC X(64).                01060000
               04  DB-ACT-NAME-ADDR-3         PIC X(64).                01070000
               04  DB-ACT-NAME-ADDR-4         PIC X(64).                01080000
               04  DB-ACT-NAME-ADDR-5         PIC X(64).                01090000
               04  DB-ACT-NAME-ADDR-6         PIC X(64).                01100000
               04  DB-ACT-CITY                PIC X(30).                01110000
               04  DB-ACT-STATE-CODE          PIC X(2).                 01120000
               04  DB-ACT-ZIP-CODE            PIC S9(5)   COMP-3.       01130000
               04  DB-ACT-ZIP-PLUS4           PIC S9(5)   COMP-3.       01140000
               04  DB-ACT-USE-ZIP4            PIC X(1).                 01150000
               04  DB-ACT-DELIVERY-PT-NO      PIC S9(3)   COMP-3.       01160000
               04  DB-ACT-DELIVERY-PT-CHK     PIC S9(1).                01170000
               04  DB-ACT-CARRIER-ROUTE       PIC X(4).                 01180000
               04  DB-ACT-CITIZEN-COUNTRY     PIC X(2).                 01190000
               04  DB-ACT-RESIDE-COUNTRY      PIC X(2).                 01200000
               04  DB-ACT-PROVINCE-CODE       PIC X(2).                 01210000
               04  DB-ACT-FOREIGN-POSTAL      PIC X(10).                01220000
               04  DB-ACT-ALPHA-KEY-1         PIC X(30).                01230000
               04  DB-ACT-ALPHA-KEY-2         PIC X(30).                01240000
               04  DB-ACT-ALPHA-KEY-3         PIC X(30).                01250000
               04  DB-ACT-ALPHA-KEY-4         PIC X(30).                01260000
               04  DB-ACT-ALPHA-KEY-5         PIC X(30).                01270000
               04  DB-ACT-HOME-PHONE          PIC S9(11)  COMP-3.       01280000
               04  DB-ACT-BUSINESS-PHONE      PIC S9(11)  COMP-3.       01290000
               04  DB-ACT-PHONE-EXTENSION     PIC S9(5)   COMP-3.       01300000
               04  DB-ACT-OTHER-PHONE         PIC S9(11)  COMP-3.       01310000
               04  DB-ACT-FAX-PHONE           PIC S9(11)  COMP-3.       01320000
               04  DB-ACT-CELL-PHONE          PIC S9(11)  COMP-3.       01330000
               04  DB-ACT-NO-TAXID            PIC X(1).                 01340000
               04  DB-ACT-TAXID-FORMAT        PIC X(1).                 01350000
               04  DB-ACT-TAXID               PIC S9(9)   COMP-3.       01360000
               04  DB-ACT-ACTIVE-YM           PIC S9(5)   COMP-3.       01370000
               04  DB-ACT-LAST-STMT-MY        PIC S9(5)   COMP-3.       01380000
               04  DB-ACT-OPEN-DATE           PIC X(10).                01390000
               04  DB-ACT-CHANGE-DATE         PIC X(10).                01400000
               04  DB-ACT-UPDATE-SOURCE-ID    PIC X(8).                 01410000
               04  DB-ACT-UPDATE-SOURCE       PIC X(8).                 01420000
               04  DB-ACT-UPDATE-TERMID       PIC X(4).                 01430000
               04  DB-ACT-CASH-INSTR          PIC X(2).                 01440000
               04  DB-ACT-CASH-INSTR-2        PIC X(2).                 01450000
               04  DB-ACT-FOREIGN-INSTR       PIC X(2).                 01460000
               04  DB-ACT-MARGIN-INSTR        PIC X(2).                 01470000
               04  DB-ACT-DIVIDEND-INSTR      PIC X(1).                 01480000
               04  DB-ACT-INSTITUTION-CODE    PIC X(1).                 01490000
               04  DB-ACT-INST-OVERRIDE       PIC X(1).                 01500000
               04  DB-ACT-WITHHOLD            PIC X(1).                 01510000
               04  DB-ACT-WITHHOLD-DATE       PIC X(10).                01520000
               04  DB-ACT-WITHHOLD-TAX        PIC X(1).                 01530000
               04  DB-ACT-WITHHOLD-TAX-RATE   PIC S9(3)V9(6)  COMP-3.   01540000
               04  DB-ACT-TEFRA               PIC X(1).                 01550000
               04  DB-ACT-W9-STATUS           PIC X(1).                 01560000
               04  DB-ACT-W8-STATUS           PIC X(1).                 01570000
               04  DB-ACT-W8-MAIL-YR          PIC S9(3)   COMP-3.       01580000
               04  DB-ACT-NY-TAX              PIC X(1).                 01590000
               04  DB-ACT-INVESTMENT-OBJ      PIC X(1).                 01600000
               04  DB-ACT-OPTION-INV-OBJ      PIC X(1).                 01610000
               04  DB-ACT-CLASS               PIC X(1).                 01620000
               04  DB-ACT-EMP-CLASS-CODE      PIC X(5).                 01630000
               04  DB-ACT-DIRECTOR            PIC X(1).                 01640000
               04  DB-ACT-DISCLOSURE          PIC X(1).                 01650000
               04  DB-ACT-DISCRETION          PIC X(1).                 01660000
               04  DB-ACT-STOCK-COMM-SCHED    PIC S9(3)   COMP-3.       01670000
               04  DB-ACT-BOND-COMM-SCHED     PIC S9(3)   COMP-3.       01680000
               04  DB-ACT-OPTION-COMM-SCHED   PIC S9(3)   COMP-3.       01690000
               04  DB-ACT-FUTURE-COMM-SCHED   PIC S9(3)   COMP-3.       01700000
               04  DB-ACT-STOCK-DISCOUNT      PIC S9(1)V9(2)  COMP-3.   01710000
               04  DB-ACT-BOND-DISCOUNT       PIC S9(1)V9(2)  COMP-3.   01720000
               04  DB-ACT-OPTION-DISCOUNT     PIC S9(1)V9(2)  COMP-3.   01730000
               04  DB-ACT-FUTURE-DISCOUNT     PIC S9(1)V9(2)  COMP-3.   01740000
               04  DB-ACT-COMM-OVERRIDE-SW    PIC X(1).                 01750000
               04  DB-ACT-DISCOUNT-SHR-EXEMPT PIC X(1).                 01760000
               04  DB-ACT-POSTAGE-AMT         PIC S9(16)V9(2)  COMP-3.  01770000
               04  DB-ACT-DIVIDEND-ROUND      PIC X(1).                 01780000
               04  DB-ACT-RIN-LABEL           PIC X(1).                 01790000
               04  DB-ACT-REINVEST            PIC X(1).                 01800000
               04  DB-ACT-ACCT-CATEGORY       PIC S9(3)   COMP-3.       01810000
               04  DB-ACT-TRANSACTION-LEVEL   PIC S9(3)   COMP-3.       01820000
               04  DB-ACT-NEW-ACCT-FORM       PIC S9(1).                01830000
               04  DB-ACT-JOINT-ACCT-FORM     PIC S9(1).                01840000
               04  DB-ACT-PARTNERSHIP-FORM    PIC S9(1).                01850000
               04  DB-ACT-CORPORATION-FORM    PIC S9(1).                01860000
               04  DB-ACT-MARGIN-FORM         PIC S9(1).                01870000
               04  DB-ACT-OPTION-FORM         PIC S9(1).                01880000
               04  DB-ACT-ORGANIZATION-FORM   PIC S9(1).                01890000
               04  DB-ACT-TRUST-ACCT-FORM     PIC S9(1).                01900000
               04  DB-ACT-SOLE-PROP-FORM      PIC S9(1).                01910000
               04  DB-ACT-RESTRICT-FORM       PIC S9(1).                01920000
               04  DB-ACT-F1001-FORM          PIC X(1).                 01930000
               04  DB-ACT-REPO-FORM           PIC X(1).                 01940000
               04  DB-ACT-F121-FORM           PIC X(1).                 01950000
               04  DB-ACT-COLLATERAL-FORM     PIC X(1).                 01960000
               04  DB-ACT-STATE-REG-EXEMPT    PIC X(1).                 01970000
               04  DB-ACT-ALTERNATE-STATE     PIC X(2).                 01980000
               04  DB-ACT-RESTRICT-CODE       PIC X(1).                 01990000
               04  DB-ACT-RESTRICT-DATE       PIC X(10).                02000000
               04  DB-ACT-DOC-RESTRICT-CODE   PIC X(1).                 02010000
               04  DB-ACT-DOC-RESTRICT-DATE   PIC X(10).                02020000
               04  DB-ACT-ORDR-ONLY           PIC X(1).                 02030000
               04  DB-ACT-PREVENT-PRIN-TRADES PIC X(1).                 02040000
               04  DB-ACT-IPO-ELIGIBLE        PIC X(1).                 02050000
               04  DB-ACT-ACCT-TYPE-IND       PIC X(1).                 02060000
051113         04  FILLER                     PIC X(13).                02070000
               04  DB-ACT-NO-AUTO-CANCEL      PIC X(1).                 02080000
               04  DB-ACT-AVG-PRICE           PIC X(1).                 02090000
               04  DB-ACT-SPECIAL-PROCESSING  PIC X(1).                 02100000
               04  DB-ACT-CONFIRM-SUPPRESS    PIC X(1).                 02110000
               04  DB-ACT-CONFIRM-COPIES      PIC S9(1).                02120000
               04  DB-ACT-AVG-PRICE-ORDERS    PIC X(1).                 02130000
               04  DB-ACT-INVESTOR-OVERRIDE   PIC X(1).                 02140000
               04  DB-ACT-BOLT-IND            PIC X(1).                 02150000
               04  DB-ACT-SEGREGATION         PIC X(1).                 02160000
               04  DB-ACT-STK-LOAN-TRANS      PIC X(1).                 02170000
               04  DB-ACT-NETWORK-ELIG        PIC X(1).                 02180000
               04  DB-ACT-MANAGEMENT-GROUP    PIC X(4).                 02190000
               04  DB-ACT-TPA-NUMBER          PIC X(4).                 02200000
               04  DB-ACT-NAV                 PIC X(1).                 02210000
               04  DB-ACT-NAVR-SW             PIC X(1).                 02220000
               04  DB-ACT-ACCT-STATUS-IND     PIC X(1).                 02230000
               04  DB-ACT-DLR-SW1             PIC X(1).                 02240000
               04  DB-ACT-DLR-SW2             PIC X(1).                 02250000
               04  DB-ACT-DLR-SW3             PIC X(1).                 02260000
               04  DB-ACT-DLR-SW4             PIC X(1).                 02270000
               04  DB-ACT-DLR-SW5             PIC X(1).                 02280000
               04  DB-ACT-DLR-SW6             PIC X(1).                 02290000
               04  DB-ACT-INV1                PIC X(1).                 02300000
               04  DB-ACT-MISC1               PIC X(1).                 02310000
               04  DB-ACT-MISC2               PIC X(1).                 02320000
               04  DB-ACT-MISC3               PIC X(1).                 02330000
               04  DB-ACT-MISC4               PIC X(1).                 02340000
               04  DB-ACT-MISC5               PIC X(1).                 02350000
               04  DB-ACT-MISC6               PIC X(1).                 02360000
               04  DB-ACT-EXCLUDE-MAIL-LINE1  PIC X(1).                 02370000
               04  DB-ACT-EXCLUDE-MAIL-LINE2  PIC X(1).                 02380000
               04  DB-ACT-EXCLUDE-MAIL-LINE3  PIC X(1).                 02390000
               04  DB-ACT-SHIP-DUP-CONFIRM    PIC S9(1).                02400000
               04  DB-ACT-SPHD-REGISTRATION   PIC S9(1).                02410000
               04  DB-ACT-SPHD-DELIVERY       PIC S9(1).                02420000
               04  DB-ACT-SPHD-PICKUP         PIC S9(1).                02430000
               04  DB-ACT-SHIP-DUP-STMT       PIC S9(1).                02440000
               04  DB-ACT-SPHD-DIVIDEND-CHK   PIC S9(1).                02450000
               04  DB-ACT-CMA                 PIC X(1).                 02460000
               04  DB-ACT-VIOL-CODE           PIC X(1).                 02470000
031409*THIS FIELD IS USED TO INDICATE THE EXISTENCE OF FIRM/ACCT ON PAD 02480000
031409*X-INDICATES EXISTENCE OF PAD RECORD. OTHERWISE IT IS SPACES.     02490000
               04  DB-ACT-HIST-CODE           PIC X(1).                 02500000
               04  DB-ACT-ID-CONFIRM          PIC X(1).                 02510000
               04  DB-ACT-ACH-INCOME          PIC X(1).                 02520000
               04  DB-ACT-ACH-CREDIT          PIC X(1).                 02530000
               04  DB-ACT-ACH-DEBIT           PIC X(1).                 02540000
               04  DB-ACT-BRR-ELIGIBLE        PIC X(1).                 02550000
               04  DB-ACT-MMF-FUND-SEC-NO     PIC S9(9)   COMP-3.       02560000
               04  DB-ACT-MMF-SVCFEE          PIC X(1).                 02570000
               04  DB-ACT-MMF-SWEEP           PIC X(1).                 02580000
               04  DB-ACT-MMF-INITIAL-PURCH   PIC X(1).                 02590000
               04  DB-ACT-MMF-SELL-FIRST-SEC-NO                         02600000
                                              PIC S9(9)   COMP-3.       02610000
               04  DB-ACT-MMF-CASH-DIV        PIC X(1).                 02620000
               04  DB-ACT-MMF-CASH-ONLY       PIC X(1).                 02630000
               04  DB-ACT-OUTSIDE-MMF         PIC X(1).                 02640000
               04  DB-ACT-RESTING-BALANCE     PIC S9(16)V9(2)  COMP-3.  02650000
               04  DB-ACT-CRINT               PIC S9(1).                02660000
               04  DB-ACT-CRINT-NOPOST        PIC X(1).                 02670000
               04  DB-ACT-INTR                PIC X(1).                 02680000
               04  DB-ACT-NO-CHRG-MARGIN-INT  PIC X(1).                 02690000
               04  DB-ACT-OPTN-MRG-LMT        PIC S9(16)V9(2)  COMP-3.  02700000
               04  DB-ACT-NO-CHRG-SEC-FEE     PIC X(1).                 02710000
               04  DB-ACT-PRINCIPAL           PIC X(1).                 02720000
               04  DB-ACT-ACCT-ACCESS-IND     PIC X(1).                 02730000
               04  DB-ACT-ACAT-TERM-FEE       PIC X(1).                 02740000
               04  DB-ACT-BYPASS-REJECT-ACAT  PIC X(1).                 02750000
               04  DB-ACT-ERISA-CODE          PIC X(1).                 02760000
               04  DB-ACT-NO-REBATE-IND       PIC X(1).                 02770000
               04  DB-ACT-MANAGED-ACCT-NO     PIC S9(3)   COMP-3.       02780000
               04  DB-ACT-PORTFOLIO-PRINT     PIC X(2).                 02790000
               04  DB-ACT-WRAP-PLAN           PIC X(6).                 02800000
               04  DB-ACT-GENERIC-USER        PIC S9(5)   COMP-3.       02810000
               04  DB-ACT-BILL-BYPASS         PIC X(1).                 02820000
               04  DB-ACT-NO-JOURNAL-IND      PIC X(1).                 02830000
               04  DB-ACT-IRA-TERM-DATE       PIC X(10).                02840000
               04  DB-ACT-IRA-TERM-CODE       PIC X(1).                 02850000
               04  DB-ACT-REMIC-CMO-SW        PIC X(1).                 02860000
               04  DB-ACT-MAIL-DIVERT-CODE    PIC X(4).                 02870000
               04  DB-ACT-PRODUCT-CLASS       PIC X(4).                 02880000
               04  DB-ACT-STATEMENT-CODE      PIC X(1).                 02890000
               04  DB-ACT-ACCESS-EQ-DELIVERY  PIC X(1).                 02900000
               04  DB-ACT-CASH-DEBIT-OVERRIDE PIC X(1).                 02910000
               04  DB-ACT-LAST-REVIEW-CODE    PIC X(1).                 02920000
               04  DB-ACT-LAST-REVIEW-DATE    PIC X(10).                02930000
               04  DB-ACT-CUST-NASD-SYM       PIC X(5).                 02940000
               04  DB-ACT-NON-CUST            PIC X(1).                 02950000
               04  DB-ACT-PRIME-BROKER        PIC X(1).                 02960000
               04  DB-ACT-MUNI-CNS-PART       PIC X(1).                 02970000
               04  DB-ACT-BROKER-NO           PIC S9(5)   COMP-3.       02980000
               04  DB-ACT-FLIP-EXCH           PIC S9(3)   COMP-3.       02990000
               04  DB-ACT-CORR-CLEAR          PIC X(2).                 03000000
               04  DB-ACT-FLIP-SELL-COMM      PIC S9(3)V9(6)  COMP-3.   03010000
               04  DB-ACT-FLIP-BUY-COMM       PIC S9(3)V9(6)  COMP-3.   03020000
               04  DB-ACT-LOAN-RATE           PIC S9(3)V9(6)  COMP-3.   03030000
               04  DB-ACT-BORROW-RATE         PIC S9(3)V9(6)  COMP-3.   03040000
               04  DB-ACT-MASTER-ACCT         PIC X(1).                 03050000
               04  DB-ACT-NCC-NO              PIC S9(5)   COMP-3.       03060000
               04  DB-ACT-FBO                 PIC X(1).                 03070000
               04  DB-ACT-NASDAQ-SYMBOL       PIC X(5).                 03080000
               04  DB-ACT-OTHER-DEP           PIC X(6).                 03090000
               04  DB-ACT-GSCC-NO             PIC S9(5)   COMP-3.       03100000
               04  DB-ACT-MBSCC-NO            PIC X(5).                 03110000
               04  DB-ACT-CMTA-NO             PIC S9(5)   COMP-3.       03120000
               04  DB-ACT-SOCIAL-CODE         PIC X(2).                 03130000
041809*** NEW FIELD FOR CRNCY SEQ, REDUECE THE FILLER BY 3 WHEN IN USE  03140000
041809*** FROM 368 TO 365                                               03150000
041809         04  DB-ACT-DEFAULT-CRNCY-SEQNO PIC S9(5)   COMP-3.       03160000
091209         04  DB-ACT-REP-KEY-1           PIC X(4).                 03170000
091209         04  DB-ACT-REP-KEY-2           PIC X(4).                 03180000
091209         04  DB-ACT-REP-KEY-3           PIC X(4).                 03190000
091209         04  DB-ACT-REP-KEY-4           PIC X(4).                 03200000
091209         04  DB-ACT-REP-KEY-5           PIC X(4).                 03210000
081311         04  DB-ACT-INST-ACCT-IND       PIC X(1).                 03220000
091512*  THESE ALP FIELDS WERE ADDED FOR THE VIEW 30 KEY 1 UPDATE AND   03230000
091512*  THEY ARE NOT INTENDED FOR USE WITH ANY OTHER VIEW PURPOSES     03240000
091512         04  DB-ACT-ALP1-FIRM-NO        PIC S9(3)   COMP-3.       03250000
091512         04  DB-ACT-ALP1-SUB-NO         PIC S9(3)   COMP-3.       03260000
091512         04  DB-ACT-ALP2-FIRM-NO        PIC S9(3)   COMP-3.       03270000
091512         04  DB-ACT-ALP2-SUB-NO         PIC S9(3)   COMP-3.       03280000
091512         04  DB-ACT-ALP3-FIRM-NO        PIC S9(3)   COMP-3.       03290000
091512         04  DB-ACT-ALP3-SUB-NO         PIC S9(3)   COMP-3.       03300000
091512         04  DB-ACT-ALP4-FIRM-NO        PIC S9(3)   COMP-3.       03310000
091512         04  DB-ACT-ALP4-SUB-NO         PIC S9(3)   COMP-3.       03320000
091512         04  DB-ACT-ALP5-FIRM-NO        PIC S9(3)   COMP-3.       03330000
091512         04  DB-ACT-ALP5-SUB-NO         PIC S9(3)   COMP-3.       03340000
030713         04  DB-ACT-INV-OPTION-CODE     PIC X(1).                 03350000
111613         04  DB-ACT-TIER-IND            PIC X(1).                 03360000
112115         04  DB-ACT-NATURAL-PERSON      PIC X(1).                 03370000
112115         04  FILLER                     PIC X(267).               03380000
031514     03  DB-ACT-INDICATOR-FIELDS.                                 03390000
031514         04  DB-ACT-PORTFOLIO-PRINT-IND PIC S9(4)   COMP.         03400000
031514         04  DB-ACT-MANAGED-ACCT-NO-IND PIC S9(4)   COMP.         03410000
031514         04  FILLER                     PIC X(50).                03420000
      *  THE FOLLOWING FIELDS ARE NON-STANDARD. THEY ARE NEEDED TO      03430000
      *  BUILD PACKED FIELDS ON THE ACT DATABASE WHERE EACH BYTE OF     03440000
      *  THE PACKED FIELD HAS A SPECFIC MEANING.                        03450000
           03  DB-ACT-WORK-FIELDS.                                      03460000
               04  DB-ACT-FORMS               PIC 9(11).                03470000
               04  DB-ACT-SPHD                PIC 9(9).                 03480000
