      *COPY DEACCT2.                                                    00010000
      *                                                                 00010100
      *    RECORD LENGTH = 1621                                         00010200
      *                                                                 00010300
      ***************************************************************** 00020000
      *        CHANGE HISTORY                                         * 00030000
      ***************************************************************** 00040000
071418* SHOBITH VIJAYA JEPPU                                            00040100
071418* 07/14/18 HOST046195 WR44988                                     00040200
071418*          ADD NEW FIELD FOR MMF SYMBOL                           00040300
071418*          SYNCH UP THE STRUCTURE WITH DEACCT2                    00040400
071517* PAUL FAIT                                                       00060203
071517* 07/15/2017 HOST044261 WR39325                                   00060303
071517*          NEW WEB COPYBOOK FOR IIB (DE30224I)                    00060403
071517*          WEB VERSION OF DEACCT2                                 00060503
      ***************************************************************** 00190000
      **                                                              * 00200000
071517 01  WEB-DEACCT2-PARAMETERS.                                      00201003
071418     02  ACCT2-KEY-ACCT-RECORD.                                   00211004
               04  NAFIRM           PICTURE 9(3)                        00212001
                                        COMPUTATIONAL-3.                00213001
               04  NAACCT-NO        PICTURE 9(9)                        00214001
                                        COMPUTATIONAL-3.                00215001
               04  NASUB            PICTURE 9(3)                        00216001
                                        COMPUTATIONAL-3.                00217001
               04  NANO9-KEY        PICTURE 9(9)                        00218001
                                        COMPUTATIONAL-3.                00219001
               04  NAREP            PICTURE X(4).                       00220001
               04  FILLER001 REDEFINES NAREP.                           00230001
                   05  NABRCH       PICTURE X(2).                       00240001
                   05  NAMAN        PICTURE X(2).                       00241001
               04  NA-CONV-ACCT     PICTURE 9(15).                      00242001
               04  NA-CONV-ACCTX                                        00243001
                             REDEFINES NA-CONV-ACCT                     00243101
                                    PICTURE X(15).                      00243201
               04  FILLER002 REDEFINES NA-CONV-ACCT.                    00243301
                   05  NA-CONV-FIRMX                                    00243401
                                    PICTURE X(3).                       00243501
                   05  NA-CONV-ACCOUNTX                                 00243601
                                    PICTURE X(12).                      00243701
               04  FILLER003 REDEFINES NA-CONV-ACCT.                    00243801
                   05  NA-CONV-FIRM PICTURE 9(3).                       00243901
                   05  NA-CONV-ACCOUNT                                  00244001
                                    PICTURE 9(12).                      00244101
               04  NAZIP-CODE       PICTURE 9(5)                        00244201
                                        COMPUTATIONAL-3.                00244301
               04  NAZIP-REST       PICTURE 9(5)                        00244401
                                        COMPUTATIONAL-3.                00244501
               04  NAFOREIGN-ZIP    PICTURE X(10).                      00244601
               04  NA-NAMELINES     PICTURE 9(1).                       00244701
               04  NALINES.                                             00244801
                   05  NAONE        PICTURE X(30).                      00244901
                   05  NATWO        PICTURE X(30).                      00245001
                   05  NATHREE      PICTURE X(30).                      00245101
                   05  NAFOUR       PICTURE X(30).                      00245201
                   05  NAFIVE       PICTURE X(30).                      00245301
                   05  NASIX        PICTURE X(30).                      00245401
               04  FILLER004 REDEFINES NALINES.                         00245501
                   05  NA-LINE-X    PICTURE X(30)                       00245601
                                        OCCURS 6 TIMES.                 00245701
               04  NASUB-ALPHA-KEYS.                                    00245801
                   05  NA-SUBFIRM-KEY1.                                 00245901
                       06  NASUB-KEY1                                   00246001
                                    PICTURE 9(3)                        00246101
                                        COMPUTATIONAL-3.                00246201
                       06  NAFIRM-KY1.                                  00246301
                           07  NAFIRM-KEY1                              00246401
                                    PICTURE 9(3)                        00246501
                                        COMPUTATIONAL-3.                00246601
                           07  NA-KEY1                                  00246701
                                    PICTURE X(20).                      00246801
                   05  NA-SUBFIRM-KEY2.                                 00246901
                       06  NASUB-KEY2                                   00247001
                                    PICTURE 9(3)                        00247101
                                        COMPUTATIONAL-3.                00247201
                       06  NAFIRM-KY2.                                  00247301
                           07  NAFIRM-KEY2                              00247401
                                    PICTURE 9(3)                        00247501
                                        COMPUTATIONAL-3.                00247601
                           07  NA-KEY2                                  00247701
                                    PICTURE X(20).                      00247801
                   05  NA-SUBFIRM-KEY3.                                 00247901
                       06  NASUB-KEY3                                   00248001
                                    PICTURE 9(3)                        00248101
                                        COMPUTATIONAL-3.                00248201
                       06  NAFIRM-KY3.                                  00248301
                           07  NAFIRM-KEY3                              00248401
                                    PICTURE 9(3)                        00248501
                                        COMPUTATIONAL-3.                00248601
                           07  NA-KEY3                                  00248701
                                    PICTURE X(20).                      00248801
                   05  NA-SUBFIRM-KEY4.                                 00248901
                       06  NASUB-KEY4                                   00249001
                                    PICTURE 9(3)                        00249101
                                        COMPUTATIONAL-3.                00249201
                       06  NAFIRM-KY4.                                  00249301
                           07  NAFIRM-KEY4                              00249401
                                    PICTURE 9(3)                        00249501
                                        COMPUTATIONAL-3.                00249601
                           07  NA-KEY4                                  00249701
                                    PICTURE X(20).                      00249801
                   05  NA-SUBFIRM-KEY5.                                 00249901
                       06  NASUB-KEY5                                   00250001
                                    PICTURE 9(3)                        00250101
                                        COMPUTATIONAL-3.                00250201
                       06  NAFIRM-KY5.                                  00250301
                           07  NAFIRM-KEY5                              00250401
                                    PICTURE 9(3)                        00250501
                                        COMPUTATIONAL-3.                00250601
                           07  NA-KEY5                                  00250701
                                    PICTURE X(20).                      00250801
               04  FILLER005 REDEFINES NASUB-ALPHA-KEYS.                00250901
                   05  NA-ALPHA-X                                       00251001
                                        OCCURS 5 TIMES.                 00251101
                       06  NA-ALPHA-SUB-X                               00251201
                                    PICTURE 9(3)                        00251301
                                        COMPUTATIONAL-3.                00251401
                       06  NA-ALPHA-FIRM-X                              00251501
                                    PICTURE 9(3)                        00251601
                                        COMPUTATIONAL-3.                00251701
                       06  NA-ALPHA-KEYS-X                              00251801
                                    PICTURE X(20).                      00251901
               04  NAPHONE          PICTURE 9(11)                       00252001
                                        COMPUTATIONAL-3.                00252101
               04  NAPHONE2         PICTURE 9(11)                       00252201
                                        COMPUTATIONAL-3.                00252301
               04  NAPHONE3         PICTURE 9(11)                       00252401
                                        COMPUTATIONAL-3.                00252501
               04  NA-STATE-CODE    PICTURE X(2).                       00252601
               04  NA-CNTRY         PICTURE X(2).                       00252701
               04  NA-PROV          PICTURE X(2).                       00252801
               04  NA-CNTRY-OF-RES  PICTURE X(2).                       00252901
               04  NA-NO-AUTO-CXL   PICTURE X(1).                       00253001
               04  NA-NOID          PICTURE X(1).                       00253101
               04  NAIDFORMAT       PICTURE X(1).                       00253201
               04  NA-W8-MAILING-YR PICTURE 9(3)                        00253301
                                        COMPUTATIONAL-3.                00253401
               04  NA-W8-MAILING-YRX                                    00253501
                             REDEFINES NA-W8-MAILING-YR                 00253601
                                    PICTURE X(2).                       00253701
               04  NAID             PICTURE S9(9)                       00253801
                                        COMPUTATIONAL-3.                00253901
               04  NAACTIVE-YM      PICTURE 9(5)                        00254001
                                        COMPUTATIONAL-3.                00254101
012998*---------------------------------------------------------------* 00254201
012998* DATES ARE MOSTLY IN YYMM FORMAT - I ALSO FOUND:               * 00254301
012998*    INVALID DATA; ZEROS; LOOKS LIKE A 'MAINT' IS NEEDED        * 00254401
012998*---------------------------------------------------------------* 00254501
               04  NAOPEN-MDY       PICTURE 9(9)                        00254601
                                        COMPUTATIONAL-3.                00254701
012698*---------------------------------------------------------------* 00254801
012698* DATES ARE MOSTLY IN MMDDYY FORMAT - I ALSO FOUND:             * 00254901
012698*    INVALID DATA; ZEROS; MMYY; DDYYMM?                         * 00255001
012698*---------------------------------------------------------------* 00255101
               04  NACHANGE-MDY     PICTURE 9(9)                        00255201
                                        COMPUTATIONAL-3.                00255301
012698*---------------------------------------------------------------* 00255401
012698* DATES ARE MOSTLY IN MMDDYY FORMAT - I ALSO FOUND:             * 00255501
012698*    INVALID MONTH; INVALID DAY                                 * 00255601
012698*---------------------------------------------------------------* 00255701
               04  NASTDINST.                                           00255801
                   05  NACASHSI.                                        00255901
                       06  NACSI-ONE                                    00256001
                                    PICTURE X(1).                       00256101
                           88  CASH-REG-MAIL                            00256201
                                           VALUE '1'.                   00256301
                           88  CASH-REG-HOLD                            00256401
                                           VALUE '2'.                   00256501
                           88  CASH-REG-CALL                            00256601
                                           VALUE '3'.                   00256701
                           88  CASH-STREET-NAME                         00256801
                                           VALUE '4'.                   00256901
                           88  CASH-DVP    VALUE '5'.                   00257001
                           88  CASH-INSTRUCTIONS                        00257101
                                           VALUE '6'.                   00257201
                       06  NACSI-TWO                                    00257301
                                    PICTURE X(1).                       00257401
                           88  CASH-NET-ACCOUNT                         00257501
                                           VALUE 'A'.                   00257601
                           88  CASH-PAY-PAY                             00257701
                                           VALUE 'B'.                   00257801
                           88  CASH-HOLD-FUNDS                          00257901
                                           VALUE 'C'.                   00258001
                   05  NAMARGSI.                                        00258101
                       06  NAMSI-ONE                                    00258201
                                    PICTURE X(1).                       00258301
                           88  MARG-REG-MAIL                            00258401
                                           VALUE '1'.                   00258501
                           88  MARG-REG-HOLD                            00258601
                                           VALUE '2'.                   00258701
                           88  MARG-REG-CALL                            00258801
                                           VALUE '3'.                   00258901
                           88  MARG-STREET-NAME                         00259001
                                           VALUE '4'.                   00259101
                           88  MARG-DVP    VALUE '5'.                   00259201
                           88  MARG-INSTRUCTIONS                        00259301
                                           VALUE '6'.                   00259401
                       06  NAMSI-TWO                                    00259501
                                    PICTURE X(1).                       00259601
                           88  MARG-NET-ACCOUNT                         00259701
                                           VALUE 'A'.                   00259801
                           88  MARG-PAY-PAY                             00259901
                                           VALUE 'B'.                   00260001
                           88  MARG-HOLD-FUNDS                          00260101
                                           VALUE 'C'.                   00260201
                   05  NADIVSI      PICTURE X(1).                       00260301
                       88  DIV-CREDIT      VALUE '1'.                   00260401
                       88  DIV-DAILY       VALUE '2'.                   00260501
                       88  DIV-WEEKLY      VALUE '3'.                   00260601
                       88  DIV-SEMIMONTHLY VALUE '4'.                   00260701
                       88  DIV-MONTHLY     VALUE '5'.                   00260801
               04  NA-SECOND-CSI    PICTURE X(2).                       00260901
               04  NA-WITHHOLD      PICTURE X(1).                       00261001
               04  NA-W9-REQUEST    PICTURE X(1).                       00261101
               04  NA-W8-REQUEST    PICTURE X(1).                       00261201
               04  NA-TEFRA         PICTURE X(1).                       00261301
               04  NA-PRINCIPAL     PICTURE X(1).                       00261401
               04  NA-NRA-OVERRIDE  PICTURE X(1).                       00261501
               04  NA-F1001-REQUEST                                     00261601
                             REDEFINES NA-NRA-OVERRIDE                  00261701
                                    PICTURE X(1).                       00261801
               04  NACODES.                                             00261901
                   05  NAINST       PICTURE X(1).                       00262001
                   05  NANYTAX      PICTURE X(1).                       00262101
                   05  NAINVOBJ     PICTURE X(1).                       00262201
                   05  NACLASS      PICTURE X(1).                       00262301
               04  NA-NETWORK-ELIG  PICTURE X(1).                       00262401
               04  NAFORMS          PICTURE 9(11)                       00262501
                                        COMPUTATIONAL-3.                00262601
               04  MISC6            PICTURE X(1).                       00262701
               04  NA-INVESTORS-SW                                      00262801
                             REDEFINES MISC6                            00262901
                                    PICTURE X(1).                       00263001
               04  NASPHDLG         PICTURE 9(9)                        00263101
                                        COMPUTATIONAL-3.                00263201
               04  NA-CMA           PICTURE X(1).                       00263301
               04  NA-VIOL-CODE     PICTURE X(1).                       00263401
031409*THIS FIELD IS USED TO INDICATE THE EXISTENCE OF FIRM/ACCT ON PAD 00263501
031409*'X'-INDICATES EXISTENCE OF PAD RECORD. OTHERWISE IT IS SPACES.   00263601
               04  NA-HIST-CODE     PICTURE X(1).                       00263701
               04  NA-DIR-SW        PICTURE X(1).                       00263801
               04  NA-DISCL-CODE    PICTURE X(1).                       00263901
               04  NA-DIV-RND       PICTURE X(1).                       00264001
                   88  DIV-CASH-IN-LIEU    VALUE  SPACE.                00264101
                   88  DIV-ROUND-UP        VALUE 'X'.                   00264201
               04  NA-DISCR         PICTURE X(1).                       00264301
               04  NAOPTN-I-O       PICTURE X(1).                       00264401
               04  NA-AVG-PRICE     PICTURE X(1).                       00264501
               04  NA-SPECIAL-PROCESSING                                00264601
                             REDEFINES NA-AVG-PRICE                     00264701
                                    PICTURE X(1).                       00264801
               04  INV-SEL-CODES.                                       00264901
                   05  INV1         PICTURE X(1).                       00265001
                   05  MISC1        PICTURE X(1).                       00265101
                   05  MISC2        PICTURE X(1).                       00265201
                   05  MISC3        PICTURE X(1).                       00265301
                   05  MISC4        PICTURE X(1).                       00265401
                   05  MISC5        PICTURE X(1).                       00265501
               04  NA-DLR-SWITCHES-FOR-LEDGERS                          00265601
                             REDEFINES INV-SEL-CODES.                   00265701
                   05  NA-DLR-SW    PICTURE X(1)                        00265801
                                        OCCURS 6 TIMES.                 00265901
031804         04  NA-UPDATE-SOURCE-ID                                  00266001
031804                              PICTURE X(8).                       00266101
031804         04  NA-UPDATE-SOURCE                                     00266201
031804                              PICTURE X(8).                       00266301
060304         04  NA-ST-REG-EXEMPT PICTURE X(1).                       00266401
112115         04  NA-NATURAL-PERSON                                    00266501
112115                              PICTURE X(1).                       00266601
               04  NA-MMF-SWEEP     PICTURE X(1).                       00266701
               04  NA-MMF-INIT-PURCH                                    00266801
                                    PICTURE X(1).                       00266901
051113         04  FILLER           PICTURE X(1).                       00267001
               04  NA-MMF-CASH-DIV  PICTURE X(1).                       00267101
               04  NA-MMF-CASH-ONLY PICTURE X(1).                       00267201
041809*** NEW REPLACEMENT TO NA-MMF-FUNDS                               00267301
041809         04  NA-DEFAULT-CRNCY.                                    00267401
041809             05  NA-DEFAULT-CRNCY-SEQNO                           00267501
041809                              PICTURE S9(5) COMP-3.               00267601
041809             05  FILLER       PICTURE X(1).                       00267701
041809         04  NA-MMF-FUNDS  REDEFINES NA-DEFAULT-CRNCY.            00267801
041809             05  NA-MMF-FUND1 PICTURE X(1).                       00267901
041809             05  NA-MMF-FUND2 PICTURE X(1).                       00268001
041809             05  NA-MMF-FUND3 PICTURE X(1).                       00268101
041809             05  NA-MMF-FUND4 PICTURE X(1).                       00268201
041809         04  FILLER    REDEFINES NA-MMF-FUNDS.                    00268301
041809             05  NA-MMF-FUND  PICTURE X(1)                        00268401
041809                                  OCCURS 4 TIMES.                 00268501
111613         04  NA-TIER-IND      PICTURE X(1).                       00268601
               04  NA-INTR          PICTURE X(1).                       00268701
               04  NA-MARG-INT-CHG  PICTURE X(1).                       00268801
               04  NA-SEGREGATION   PICTURE X(1).                       00268901
               04  NA-CRINT         PICTURE 9(1).                       00269001
               04  NA-OPTN-MRG-LMT  PICTURE 9(7)                        00269101
                                        COMPUTATIONAL-3.                00269201
               04  NA-ERISA-CODE    PICTURE X(1).                       00269301
               04  NA-CRINT-NOPOST  PICTURE X(1).                       00269401
               04  NAFORGNSI.                                           00269501
                   05  NAFSI-ONE    PICTURE X(1).                       00269601
                       88  FORGN-REG-MAIL  VALUE  '1'.                  00269701
                       88  FORGN-REG-HOLD  VALUE  '2'.                  00269801
                       88  FORGN-REG-CALL  VALUE  '3'.                  00269901
                       88  FORGN-STREET-NAME                            00270001
                                           VALUE  '4'.                  00270101
                       88  FORGN-DVP       VALUE  '5'.                  00270201
                       88  FORGN-INSTRUCTIONS                           00270301
                                           VALUE  '6'.                  00270401
                   05  NAFSI-TWO    PICTURE X(1).                       00270501
                       88  FORGN-NET-ACCOUNT                            00270601
                                           VALUE  'A'.                  00270701
                       88  FORGN-PAY-PAY   VALUE  'B'.                  00270801
                       88  FORGN-HOLD-FUNDS                             00270901
                                           VALUE  'C'.                  00271001
               04  NASTKDISCNT      PICTURE 9(1)V9(2)                   00271101
                                        COMPUTATIONAL-3.                00271201
               04  NABNDDISCNT      PICTURE 9(1)V9(2)                   00271301
                                        COMPUTATIONAL-3.                00271401
               04  NAOPTDISCNT      PICTURE 9(1)V9(2)                   00271501
                                        COMPUTATIONAL-3.                00271601
               04  NAFUTDISCNT      PICTURE 9(1)V9(2)                   00271701
                                        COMPUTATIONAL-3.                00271801
               04  NA-CFM           PICTURE 9(1).                       00271901
030304         04  NA-IPO-ELIGIBLE  PICTURE X(1).                       00272001
               04  NA-REVERSE-REP   PICTURE X(4).                       00272101
               04  NA-ACAT-TERM-FEE-SW                                  00272201
                                    PICTURE X(1).                       00272301
100302         04  NA-REVERSE-BRS-BRANCH                                00272401
100302                              PICTURE X(4).                       00272501
102502         04  NA-REPO-REQUEST  PICTURE X(1).                       00272601
               04  NA-WITHHOLD-RYMD PICTURE 9(9)                        00272701
                                        COMPUTATIONAL-3.                00272801
               04  NA-LST-STMT-MMYY PICTURE 9(5)                        00272901
                                        COMPUTATIONAL-3.                00273001
               04  NA-MAN-KEY       PICTURE X(4).                       00273101
               04  FILLER007 REDEFINES NA-MAN-KEY.                      00273201
                   05  NA-MAN-FIRM  PICTURE 9(3)                        00273301
                                        COMPUTATIONAL-3.                00273401
                   05  NA-MANO      PICTURE 9(3)                        00273501
                                        COMPUTATIONAL-3.                00273601
                   05  NA-MANOX                                         00273701
                             REDEFINES NA-MANO                          00273801
                                    PICTURE X(2).                       00273901
               04  NA-RESTRICT-MDY  PICTURE 9(9)                        00274001
                                        COMPUTATIONAL-3.                00274101
012698*---------------------------------------------------------------* 00274201
012698* DATES ARE MOSTLY IN MMDDYY FORMAT - I ALSO FOUND:             * 00274301
012698*    999995; AND 999999                                         * 00274401
012698*---------------------------------------------------------------* 00274501
               04  NAMST-BRKR       PICTURE X(1).                       00274601
               04  NA-COMPANY-MERGE-CDE                                 00274701
                                    PICTURE X(2).                       00274801
               04  NA-PHONE-EXT     PICTURE 9(5)                        00274901
                                        COMPUTATIONAL-3.                00275001
               04  NA-RESTRICT-CDE  PICTURE X(1).                       00275101
               04  NA-NO-JOURNAL-IND                                    00275201
                                    PICTURE X(1).                       00275301
               04  NA-MMFSVCFEE     PICTURE X(1).                       00275401
               04  NA-EXCLUDE-MAIL-LINES.                               00275501
                   05  NA-EXCLMAIL-LINE1                                00275601
                                    PICTURE X(1).                       00275701
                   05  NA-EXCLMAIL-LINE2                                00275801
                                    PICTURE X(1).                       00275901
                   05  NA-EXCLMAIL-LINE3                                00276001
                                    PICTURE X(1).                       00276101
               04  NA-USE-ZIP4      PICTURE X(1).                       00276201
               04  NA-DOWNLOAD-SW   PICTURE X(1).                       00276301
               04  NA-CITY          PICTURE X(16).                      00276401
               04  NA-INST-OVERRIDE PICTURE X(1).                       00276501
               04  NA-REMIC-CMO-SW  PICTURE X(1).                       00276601
               04  NA-ACCT-CAT      PICTURE 9(3)                        00276701
                                        COMPUTATIONAL-3.                00276801
               04  NA-TRAN-LVL      PICTURE 9(3)                        00276901
                                        COMPUTATIONAL-3.                00277001
               04  NA-PRIME-BRKR    PICTURE X(1).                       00277101
               04  NA-COLLATERAL-LOAN-ACCT                              00277201
                                    PICTURE X(1).                       00277301
               04  NA-ACCT-TYPE-IND PICTURE X(1).                       00277401
               04  NA-TAX-WITHHOLDING-RATE                              00277501
                                    PICTURE SV9(5)                      00277601
                                        COMPUTATIONAL-3.                00277701
               04  NA-TAX-WITHHOLDING-IND                               00277801
                                    PICTURE X(1).                       00277901
               04  NA-PHONE-FAX     PICTURE 9(11)                       00278001
                                        COMPUTATIONAL-3.                00278101
               04  NA-REST-BAL      PICTURE S9(7)                       00278201
                                        COMPUTATIONAL-3.                00278301
               04  NA-DELIVERY-PT-NO                                    00278401
                                    PICTURE 9(2).                       00278501
               04  NA-DELIVERY-PT-CHK                                   00278601
                                    PICTURE 9(1).                       00278701
               04  NA-MANAGEMENT-GROUP                                  00278801
                                    PICTURE X(4).                       00278901
               04  NA-RIN-LABEL-SENT-SW                                 00279001
                                    PICTURE X(1).                       00279101
               04  NA-MAIL-DIVERT-CD                                    00279201
                                    PICTURE X(4).                       00279301
               04  NA-PREVENT-PRINCIPAL-TRADES                          00279401
                                    PICTURE X(1).                       00279501
               04  NA-TPA-NUMBER    PICTURE X(4).                       00279601
               04  NA-PHONE-CELL    PICTURE 9(11)                       00279701
                                        COMPUTATIONAL-3.                00279801
               04  NA-CAR-RT        PICTURE X(4).                       00279901
               04  NA-BOLT-IND      PICTURE X(1).                       00280001
               04  NA-INVESTOR-CODE PICTURE X(1).                       00280101
               04  NA-DSC-SHR-EXEMPT-SW                                 00280201
                                    PICTURE X(1).                       00280301
               04  NA-PRODUCT-CLASS PICTURE X(4).                       00280401
040300         04  NA-ALTERNATE-STATE                                   00280501
040300                              PICTURE X(2).                       00280601
051800         04  NA-NO-REBATE-IND PICTURE X(1).                       00280701
102300         04  NA-NAV           PICTURE X(1).                       00280801
102300         04  NA-NAVR-SW       PICTURE X(1).                       00280901
011801         04  NA-WRAP-PLAN     PICTURE X(6).                       00281001
               04  NA-NCHG-SEC-FEE  PICTURE X(1).                       00281101
               04  NA-STMT-CODE     PICTURE X(1).                       00281201
080703         04  NA-ACCT-STATUS-IND                                   00281301
080703                              PICTURE X(1).                       00281401
080703             88  ABANDONED           VALUE 'A'.                   00281501
080703             88  ESCHEATED           VALUE 'B'.                   00281601
080703             88  CLOSED-FLAT         VALUE 'C'.                   00281701
080703             88  CLD-NONXFER         VALUE 'D'.                   00281801
020908             88  ABAND-FLAT          VALUE 'E'.                   00281901
020908             88  ABAND-NTSF          VALUE 'F'.                   00282001
111A13             88  LOST-SEC-HOLDER     VALUE 'G'.                   00282101
111A13             88  UNRESPON-PAYEE      VALUE 'H'.                   00282201
111A13             88  LOST-N-UNRESPON     VALUE 'I'.                   00282301
020908             88  ABANDONED-ALL-TYPES VALUE 'A', 'E', 'F'.         00282401
111A13             88  ABANDONED-CLOSED    VALUE 'A' THRU 'I'.          00282501
080703             88  CLOSED              VALUE 'C', 'D'.              00282601
090403         04  NA-COMM-OVERRIDE-SW                                  00282701
090403                              PICTURE X(1).                       00282801
022804         04  NA-AVG-PRICE-IND PICTURE X(1).                       00282901
               04  NA-CMTA-KEY      PICTURE X(5).                       00283001
               04  NA-CMTA-KEY-FIELDS                                   00283101
                             REDEFINES NA-CMTA-KEY.                     00283201
                   05  NA-CMTA-FIRM PICTURE 9(3)                        00283301
                                        COMPUTATIONAL-3.                00283401
                   05  NA-CMTA-NO   PICTURE 9(5)                        00283501
                                        COMPUTATIONAL-3.                00283601
               04  NA-UPD-TERM      PICTURE X(4).                       00283701
020802         04  NA-BRS-BRANCH    PICTURE X(4).                       00283801
020802         04  NA-B4-ACATS-BRS-BRANCH                               00283901
020802                              PICTURE X(4).                       00284001
051113         04  FILLER           PICTURE X(3).                       00284101
               04  NA-BROK-KEY      PICTURE X(5).                       00284201
               04  FILLER011 REDEFINES NA-BROK-KEY.                     00284301
                   05  NABROKFIRM   PICTURE 9(3)                        00284401
                                        COMPUTATIONAL-3.                00284501
                   05  NABROKRNO    PICTURE 9(5)                        00284601
                                        COMPUTATIONAL-3.                00284701
                   05  XNABROKRNO                                       00284801
                             REDEFINES NABROKRNO                        00284901
                                    PICTURE X(3).                       00285001
               04  NALRATE          PICTURE 9(2)V9(3)                   00285101
                                        COMPUTATIONAL-3.                00285201
               04  NA-SELL-COMM                                         00285301
                             REDEFINES NALRATE                          00285401
                                    PICTURE 9(1)V9(4)                   00285501
                                        COMPUTATIONAL-3.                00285601
               04  NABRATE          PICTURE 9(2)V9(3)                   00285701
                                        COMPUTATIONAL-3.                00285801
               04  NA-BUY-COMM                                          00285901
                             REDEFINES NABRATE                          00286001
                                    PICTURE 9(1)V9(4)                   00286101
                                        COMPUTATIONAL-3.                00286201
               04  NANCCNO          PICTURE 9(5)                        00286301
                                        COMPUTATIONAL-3.                00286401
               04  NAXNCCNO  REDEFINES NANCCNO                          00286501
                                    PICTURE X(3).                       00286601
               04  NA-FBO           PICTURE X(1).                       00286701
               04  NA-NASDQ-KEY     PICTURE X(7).                       00286801
               04  FILLER012 REDEFINES NA-NASDQ-KEY.                    00286901
                   05  NANASDAQ-FIRM                                    00287001
                                    PICTURE 9(3)                        00287101
                                        COMPUTATIONAL-3.                00287201
                   05  NANASDAQ     PICTURE X(5).                       00287301
               04  NA-ODD-KEY       PICTURE X(8).                       00287401
               04  FILLER013 REDEFINES NA-ODD-KEY.                      00287501
                   05  NA-ODD-FIRM  PICTURE 9(3)                        00287601
                                        COMPUTATIONAL-3.                00287701
                   05  NA-OTHER-DEP PICTURE X(6).                       00287801
               04  NA-IDCFM         PICTURE X(1).                       00287901
               04  NA-GENERIC-USER-FLD                                  00288001
                                    PICTURE 9(5)                        00288101
                                        COMPUTATIONAL-3.                00288201
               04  NA-NO-CFM        PICTURE X(1).                       00288301
               04  NA-FLIP-EXCH     PICTURE 9(2).                       00288401
               04  NA-CORR-CLEAR                                        00288501
                             REDEFINES NA-FLIP-EXCH                     00288601
                                    PICTURE X(2).                       00288701
               04  NACOMM-SCHED-STK PICTURE 9(2).                       00288801
               04  NACOMM-SCHED-BND PICTURE 9(2).                       00288901
               04  NACOMM-SCHED-OPT PICTURE 9(2).                       00289001
               04  NACOMM-SCHED-FUT PICTURE 9(2).                       00289101
               04  NA-POSTAGE-AMT   PICTURE 9(3)V9(2)                   00289201
                                        COMPUTATIONAL-3.                00289301
               04  NA-MUNI-CNS-PART PICTURE X(1).                       00289401
               04  NA-NON-CUST      PICTURE X(1).                       00289501
               04  NA-CUST-NASD-SYM PICTURE X(5).                       00289601
               04  NA-PURGE-IND     PICTURE X(1).                       00289701
               04  NA-BILL-BYPASS   PICTURE X(1).                       00289801
               04  NA-ORDR-ONLY     PICTURE X(1).                       00289901
               04  NA-IRA-TERM-YMD  PICTURE 9(9)                        00290001
                                        COMPUTATIONAL-3.                00290101
012698*---------------------------------------------------------------* 00290201
012698* DATES ARE MOSTLY IN YYMMDD FORMAT - I ALSO FOUND PROGRAMS     * 00290301
012698* USING CCYYMMDD AND I FOUND INVALID DATA IN THE CENTURY BYTES  * 00290401
012698* THIS FIELD NEEDS TO HAVE A 'MAINT' RUN AGAINST IT             * 00290501
012698*---------------------------------------------------------------* 00290601
               04  NA-REINVEST      PICTURE X(1).                       00290701
               04  NA-CASH-DR-OVRD  PICTURE X(1).                       00290801
               04  NA-IRA-TERM-CD   PICTURE X(1).                       00290901
               04  NA-IRS-NALINE1   PICTURE 9(1).                       00291001
               04  NA-IRS-NALINE2   PICTURE 9(1).                       00291101
               04  NANCCFIRM        PICTURE 9(3)                        00291201
                                        COMPUTATIONAL-3.                00291301
               04  NAXNCCFIRM                                           00291401
                             REDEFINES NANCCFIRM                        00291501
                                    PICTURE X(2).                       00291601
               04  NA-ACH-INC       PICTURE X(1).                       00291701
               04  NA-ACH-CR        PICTURE X(1).                       00291801
               04  NA-ACH-DR        PICTURE X(1).                       00291901
               04  NA-OUTSIDE-MMF   PICTURE X(1).                       00292001
               04  NA-REP-B4-ACATS  PICTURE X(4).                       00292101
               04  NA-F121-SW       PICTURE X(1).                       00292201
               04  NA-GSCC-KEY      PICTURE X(5).                       00292301
               04  FILLER    REDEFINES NA-GSCC-KEY.                     00292401
                   05  NA-GSCC-FIRM PICTURE 9(3)                        00292501
                                        COMPUTATIONAL-3.                00292601
                   05  NA-GSCC      PICTURE 9(5)                        00292701
                                        COMPUTATIONAL-3.                00292801
                   05  XNA-GSCC                                         00292901
                             REDEFINES NA-GSCC                          00293001
                                    PICTURE X(3).                       00293101
               04  NA-MBSCC-KEY     PICTURE X(7).                       00293201
               04  FILLER    REDEFINES NA-MBSCC-KEY.                    00293301
                   05  NA-MBSCC-FIRM                                    00293401
                                    PICTURE 9(3)                        00293501
                                        COMPUTATIONAL-3.                00293601
                   05  NA-MBSCC     PICTURE X(5).                       00293701
               04  NA-EMP-CLASS-CODE                                    00293801
                                    PICTURE X(5).                       00293901
               04  NA-LST-REVIEW-CODE                                   00294001
                                    PICTURE X(1).                       00294101
               04  NA-LST-REVIEW-CYMD                                   00294201
                                    PICTURE 9(9)                        00294301
                                        COMPUTATIONAL-3.                00294401
012698*---------------------------------------------------------------* 00294501
012698* DATES ARE SUPPOSED TO BE IN CYMD FORMAT, BUT I FOUND MANY IN  * 00294601
012698* MMDDYY - SEVERAL PROGRAMD ARE 'WINDOWING' THIS FORMAT         * 00294701
012698* LOOKS LIKE THIS FIELD NEEDS A 'MAINT'                         * 00294801
012698*---------------------------------------------------------------* 00294901
               04  NA-DOC-RESTRICT-CODE                                 00295001
                                    PICTURE X(1).                       00295101
               04  NA-DOC-RESTRICT-CYMD                                 00295201
                                    PICTURE 9(9)                        00295301
                                        COMPUTATIONAL-3.                00295401
012698*---------------------------------------------------------------* 00295501
012698* DATES ARE SUPPOSED TO BE IN YMD FORMAT, BUT I FOUND MANY IN   * 00295601
012698* WHAT LOOKS LIKE 99YYMMDD                                      * 00295701
012698* LOOKS LIKE THIS FIELD NEEDS A 'MAINT'                         * 00295801
012698*---------------------------------------------------------------* 00295901
               04  NA-GROUPNO       PICTURE 9(9)                        00296001
                                        COMPUTATIONAL-3.                00296101
               04  NAREP-KEYS.                                          00296201
                   05  NAREP-KEY1.                                      00296301
                       06  NAREP-KEY1-BRANCH                            00296401
                                    PICTURE X(2).                       00296501
                       06  NAREP-KEY1-MAN                               00296601
                                    PICTURE X(2).                       00296701
                   05  NAREP-KEY2.                                      00296801
                       06  NAREP-KEY2-BRANCH                            00296901
                                    PICTURE X(2).                       00297001
                       06  NAREP-KEY2-MAN                               00297101
                                    PICTURE X(2).                       00297201
                   05  NAREP-KEY3.                                      00297301
                       06  NAREP-KEY3-BRANCH                            00297401
                                    PICTURE X(2).                       00297501
                       06  NAREP-KEY3-MAN                               00297601
                                    PICTURE X(2).                       00297701
                   05  NAREP-KEY4.                                      00297801
                       06  NAREP-KEY4-BRANCH                            00297901
                                    PICTURE X(2).                       00298001
                       06  NAREP-KEY4-MAN                               00298101
                                    PICTURE X(2).                       00298201
                   05  NAREP-KEY5.                                      00298301
                       06  NAREP-KEY5-BRANCH                            00298401
                                    PICTURE X(2).                       00298501
                       06  NAREP-KEY5-MAN                               00298601
                                    PICTURE X(2).                       00298701
               04  NAREP-KEY-TABLE                                      00298801
                             REDEFINES NAREP-KEYS.                      00298901
                   05  NAREP-KEY-X                                      00299001
                                        OCCURS 5 TIMES.                 00299101
                       06  NABRCH-KEY-X                                 00299201
                                    PICTURE X(2).                       00299301
                       06  NAMAN-KEY-X                                  00299401
                                    PICTURE X(2).                       00299501
051113         04  FILLER           PICTURE X(5).                       00299601
092106         04  NA-STK-LOAN-TRANS                                    00299701
092106                              PICTURE X.                          00299801
072006         04  NA-AED-IND       PICTURE X.                          00299901
092106         04  NA-SOCIAL-CODE   PICTURE X(2).                       00300001
081311         04  NA-INST-ACCT-IND PICTURE X(1).                       00300101
030713         04  NA-INV-OPTION-CODE                                   00300201
030713                              PICTURE X(1).                       00300301
081601         04  NA-MMF-FUND-SEC-NO                                   00300401
081601                              PICTURE 9(9)                        00300501
081601                                  COMPUTATIONAL-3.                00300601
081601         04  NA-MMF-SELL-FIRST-SEC-NO                             00300701
081601                              PICTURE 9(9)                        00300801
081601                                  COMPUTATIONAL-3.                00300901
112001         04  NA-BYPASS-REJECT-ACAT                                00301001
081701                              PICTURE X(1).                       00301101
030603         04  NA-BRR-ELIGIBLE-SW                                   00301201
                                    PICTURE X(1).                       00301301
071517     02  ACCT2-LAYOUT.                                            00301402
150                10  ACCT2-CHECKS                    PIC X(01).       00301500
168                10  ACCT2-ACTIVITY-DATE             PIC X(04).       00301600
181                10  ACCT2-ACTIVITY-FLAG             PIC X(01).       00301700
201                10  ACCT2-LGD-DISCR                 PIC X(01).       00301800
203                10  ACCT2-LGD-OPTIO                 PIC X(01).       00301900
205                10  ACCT2-REST-FORM                 PIC X(01).       00302000
206                10  ACCT2-LGD-NETELIG               PIC X(01).       00302100
207                10  ACCT2-LGD-BILLBYPASS            PIC X(01).       00303000
208                10  ACCT2-LGD-ACHINC                PIC X(01).       00310000
209                10  ACCT2-LGD-ACHCR                 PIC X(01).       00320000
210                10  ACCT2-LGD-ACHDR                 PIC X(01).       00330000
211                10  ACCT2-LGD-MARS                  PIC X(01).       00340000
214                10  ACCT2-LGD-REMICCMO              PIC X(01).       00350000
215                10  ACCT2-LGD-NOAUTOCXL             PIC X(01).       00360000
216                10  ACCT2-LGD-MGRGR                 PIC X(01).       00370000
217                10  ACCT2-LGD-MAILDIVERT            PIC X(01).       00380000
218                10  ACCT2-LGD-DOWNLOAD              PIC X(01).       00390000
219                10  ACCT2-LGD-ORDERONLY             PIC X(01).       00400000
221                10  ACCT2-LGD-ALT-MAILING-ADDR      PIC X(01).       00410000
222                10  ACCT2-LGD-IDCFM                 PIC X(01).       00420000
223                10  ACCT2-LGD-HISTORY               PIC X(01).       00430000
224                10  ACCT2-LGD-VIOLATION             PIC X(01).       00440000
225                10  ACCT2-LGD-DUPCNFM               PIC X(01).       00450000
226                10  ACCT2-LGD-REGINSTR              PIC X(01).       00460000
227                10  ACCT2-LGD-DELIVERY              PIC X(01).       00470000
228                10  ACCT2-LGD-PICKUP                PIC X(01).       00480000
229                10  ACCT2-LGD-DUPLSTMT              PIC X(01).       00490000
230                10  ACCT2-LGD-DIVCKADDR             PIC X(01).       00500000
231                10  ACCT2-LGD-CMA                   PIC X(01).       00510000
232                10  ACCT2-LGD-ALTPROXY              PIC X(01).       00520000
233                10  ACCT2-LGD-GDEL                  PIC X(01).       00530000
234                10  ACCT2-LGD-SECREST               PIC X(01).       00540000
235                10  ACCT2-LGD-BNOTICE               PIC X(01).       00550000
236                10  ACCT2-LGD-NOTEPADS              PIC X(01).       00560000
237                10  ACCT2-LGD-DIST                  PIC X(01).       00570000
238                10  ACCT2-LGD-ACHDISTCR             PIC X(01).       00580000
239                10  ACCT2-LGD-ACHDISTDR             PIC X(01).       00590000
240                10  ACCT2-LGD-WRAP                  PIC X(01).       00600000
241                10  ACCT2-LGD-INTEREST              PIC X(01).       00610000
249                10  ACCT2-DISCR-TYPE                PIC X(04).       00620000
251                10  ACCT2-ACH-CR-TYPE               PIC X(03).       00630000
252                10  ACCT2-ACH-DR-TYPE               PIC X(03).       00640000
253                10  ACCT2-MARS-TYPE                 PIC X(06).       00650000
258                10  ACCT2-CMA-TYPE                  PIC X(10).       00660000
259                10  ACCT2-GDEL-TYPE                 PIC X(05).       00670000
260                10  ACCT2-WRAP-PLAN                 PIC X(06).       00680000
261                10  ACCT2-INTEREST-PCT              PIC SV9(5).      00690000
264                10  ACCT2-DUP-CONFIRM-CODE          PIC X(01).       00700000
265                10  ACCT2-REGISTRATION-CODE         PIC X(01).       00710000
266                10  ACCT2-DELIVERY-CODE             PIC X(01).       00720000
267                10  ACCT2-PICKUP-CODE               PIC X(01).       00730000
268                10  ACCT2-DUP-STATEMENT-CODE        PIC X(01).       00740000
269                10  ACCT2-DIV-CHECK-CODE            PIC X(02).       00750000
270                10  ACCT2-CMA-CODE                  PIC X(02).       00760000
271                10  ACCT2-GENERAL-DELIVERY-CODE     PIC X(02).       00770000
272                10  ACCT2-ALT-PROXY-CODE            PIC X(02).       00780000
273                10  ACCT2-VIOLATION-CODE            PIC X(02).       00790000
274                10  ACCT2-HISTORY-CODE              PIC X(02).       00800000
275                10  ACCT2-IRA-TYPE                  PIC X(05).       00810000
276                10  ACCT2-HOB-FLAG                  PIC X(01).       00820000
277                10  ACCT2-NEW-ACCOUNT-FORM          PIC X(01).       00830000
278                10  ACCT2-JOINT-ACCOUNT-FORM        PIC X(01).       00840000
279                10  ACCT2-PARTNERSHIP-FORM          PIC X(01).       00850000
280                10  ACCT2-CORPORATE-FORM            PIC X(01).       00860000
281                10  ACCT2-MARGIN-AGREEMENT          PIC X(01).       00870000
282                10  ACCT2-OPTION-AGREEMENT          PIC X(01).       00880000
283                10  ACCT2-ORGANIZATION-FORM         PIC X(01).       00890000
284                10  ACCT2-TRUST-FORM                PIC X(01).       00900000
285                10  ACCT2-UNSEC-FORM                PIC X(01).       00910000
286                10  ACCT2-SOLE-FORM                 PIC X(01).       00920000
287                10  ACCT2-BORD-RESTRICTED           PIC X(01).       00930000
530                10  ACCT2-NAV-REASON                PIC X(01).       00940000
1153               10  ACCT2-BIRTHDATE                 PIC 9(08).       00950000
1156               10  ACCT2-PRI-E-MAIL-ADDRESS        PIC X(60).       00960000
1470               10  ACCT2-LGD-RESTING-BALANCE       PIC S9(7).       00970000
1471               10  ACCT2-LGD-THIRD-PARTY           PIC X(04).       00980000
1472               10  ACCT2-LGD-CONVERSION-CODE       PIC X(02).       00990000
1604               10  ACCT2-DDA-NUMBER                PIC X(17).       01000000
1625               10  ACCT2-ADR-EXPIRE-DATE           PIC X(08).       01010000
2145               10  ACCT2-LGD-SPAD                  PIC X(01).       01020000
2339               10  ACCT2-LGD-CLED-NOTE             PIC X(01).       01030000
2359               10  ACCT2-SEC-E-MAIL-ADDRESS        PIC X(60).       01040000
2360               10  ACCT2-RISK-TOLERANCE            PIC X(01).       01050000
2539               10  ACCT2-WRAP-MGRID                PIC X(05).       01060000
2618               10  ACCT2-COMMISSION-OVERRIDE       PIC X(01).       01070000
2725               10  ACCT2-DUPLICATE-1099-SWITCH     PIC X(01).       01080000
051113             10  FILLER                          PIC X(01).       01090000
3705               10  ACCT2-CELL-PHONE-1              PIC X(10).       01100000
3706               10  ACCT2-CELL-PHONE-2              PIC X(10).       01110000
3707               10  ACCT2-HOME-PHONE-2              PIC X(10).       01120000
3708               10  ACCT2-BUSINESS-PHONE-2          PIC X(10).       01130000
3709               10  ACCT2-FIRM-DEF-PRODUCT-CODE     PIC X(04).       01140000
3710               10  ACCT2-FIRM-DEF-PRODUCT-DESC     PIC X(100).      01150000
3717               10  ACCT2-FIRM-DEF-MANAGER-ID       PIC X(08).       01160000
3718               10  ACCT2-FIRM-DEF-MANAGER-DESC     PIC X(100).      01170000
3719               10  ACCT2-FIRM-DEF-STYLE            PIC X(08).       01180000
3720               10  ACCT2-FIRM-DEF-STYLE-DESCR      PIC X(100).      01190000
3721               10  ACCT2-FIRM-DEF-MNGD-PROD-DESC   PIC X(100).      01200000
5040               10  ACCT2-FIRM-DEF-PMA              PIC X(10).       01210000
5214               10  ACCT2-REP-TRADING-RESTR         PIC X(1).        01220000
5215               10  ACCT2-REP-EQDIV-RESTR           PIC X(1).        01230000
5216               10  ACCT2-REP-MFDIV-RESTR           PIC X(1).        01240000
5217               10  ACCT2-PERIODIC-INV-RESTR        PIC X(1).        01250000
4433               10  ACCT2-WRAP-MGR-NAME             PIC X(20).       01260000
4842               10  ACCT2-WRAP-PLAN-DESC            PIC X(20).       01270000
5379               10  ACCT2-WRAP-PLAN-ADVIS           PIC X(1).        01280000
071914             10  ACCT2-LOST-PAYEE                PIC X(1).        01290000
071418             10  ACCT2-FIDUCIARY                 PIC X(1).        01290100
071418             10  ACCT2-EXEMPTION                 PIC X(1).        01290200
071418             10  ACCT2-TBA-EXCLUDED              PIC X(1).        01290300
071418             10  ACCT2-TBA-EXEMPT                PIC X(1).        01290400
071418             10  ACCT2-MULTI-CNY-IND             PIC X(1).        01290500
071418             10  ACCT2-ACT-CMP-RVW-IND           PIC X(1).        01290600
071418             10  ACCT2-MMF-SYMBOL                PIC X(3).        01290700
