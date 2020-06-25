      *******************************************************************
      * Project code name "ORCA"
      * ����W�����Z�v�g�\�t�g�iJMA standard receipt software�j
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *****************************************************************
      *  �v���O�����C������
      * Maj/Min/Rev  �C����       ���t      ���e
      *  05.00.00    NACL-���X�[  18/06/XX  ���@�Ή��ǉ�
      *****************************************************************
      ******************************************************************
      *    �`�o�h���X�|���X�ی��ꊇ�ύX��� (��̉�)
      *                                      COPY (CPMDCXMLV3RE6.INC)
      *         �f�Ís�ד��e
      ******************************************************************
       01  XML-MEDICALV3RES6.
            03  MDCRES-CONTEXT             PIC S9(9)   BINARY.
            03  MDCRES-OBJECT              PIC X(36).
            03  MDCRES-MODE                PIC S9(9)   BINARY.
            03  MDCRES-RECORDNAME          PIC X(128).
            03  MDCRES-MEDICALRES.
      *        ���N�G�X�g�ԍ�
               05  MDCRES-REQUEST-NUMBER    PIC X(2).
      *        ���X�|���X�ԍ�
               05  MDCRES-RESPONSE-NUMBER   PIC X(2).
      *        UID�i�d�q�J���e�j
               05  MDCRES-KARTE-UID         PIC X(36).
      *        UID�i�I���J�j
               05  MDCRES-ORCA-UID          PIC X(36).
      *        ���{��
               05  MDCRES-INFORMATION-DATE  PIC X(10).
      *        ���{����
               05  MDCRES-INFORMATION-TIME  PIC X(8).
      *        �ԋp�R�[�h
               05  MDCRES-API-RESULT        PIC X(3).
      *        �ԋp���b�Z�[�W
               05  MDCRES-API-RESULT-MSG    PIC X(100).
      *
               05  MDCRES-RESKEY            PIC X(100).
      *        �f�ÔN��
               05  MDCRES-PERFORM-YM        PIC X(07).
      *        �f�É�
               05  MDCRES-APPOINT-DEP-CODE  PIC X(2).
               05  MDCRES-APPOINT-DEP-NAME  PIC X(80).
      *        ���ҏ��
               05  MDCRES-PATIENTID        PIC X(20).
               05  MDCRES-NAME             PIC X(100).
               05  MDCRES-KANANAME         PIC X(100).
               05  MDCRES-BIRTHDAY         PIC X(10).
               05  MDCRES-SEX              PIC X(1).
      * ���N�G�X�g���O�P
      *        ��f�������
               05  MDCRES-MEDICAL-LIST-G.
                   07  MDCRES-MEDICAL-LIST-TBL     OCCURS   150.
      *                �f�Ó��t
                       09  MDCRES-PERFORM-DATE     PIC X(10).
      *                �`�[�ԍ�
                       09  MDCRES-INVOICD-NUMBER        PIC  X(07).
      *                �ی��g�����ԍ�
                       09  MDCRES-MED-COMBINATION-NUMBER   PIC X(04).
      *                �A��
                       09  MDCRES-MED-RENNUM             PIC X(01).
      *                �ی��g�������S�����i�O���j
                       09  MDCRES-MED-COMBINATION-RATE  PIC X(03).
      *                �ی��g���� �L���J�n��
                       09  MDCRES-MED-START-DATE        PIC X(10).
      *                �ی��g���� �L���I����
                       09  MDCRES-MED-END-DATE          PIC X(10).
      *                �ی��̎��
                       09  MDCRES-MED-INSURANCE-CLASS   PIC X(03).
      *                �ی��̎�ޖ���
                       09  MDCRES-MED-INSURANCE-NAME    PIC X(20).
      *                �ی��Ҕԍ�
                       09  MDCRES-MED-INSURANCE-NUMBER  PIC X(08).
      *                ������
                       09  MDCRES-MED-INSURANCE-INFO 
                                              OCCURS  4 TIMES.
      *                 ����̎��
                        11  MDCRES-MED-INSU-CLASS     PIC X(03).
      *                 ����̎�ޖ���
                        11  MDCRES-MED-INSU-NAME      PIC X(20).
      *                 ���S�Ҕԍ�
                        11  MDCRES-MED-PROVIDER-NUMBER     PIC X(08).
      *                 �󋋎Ҕԍ�
                        11  MDCRES-MED-RECIPIENT-NUMBER    PIC X(20).
      *                �ی��g�����폜�敪�i�P�F�폜�A�Q�F���ԊO
      *                                    �R�F�O����\���j
                       09  MDCRES-MED-COMBINATION-MODE  PIC X(1).
      *H30.6
      *                ���@���ܗ��̂ݎZ��
                       09  MDCRES-CHOZAI-AREA.
                           11  MDCRES-CHOZAI-ONLY      PIC X(10).
      *!!
      * ���N�G�X�g���O�P
      *        �ی��g�����ꗗ�ԋp
               05  MDCRES-INSURANCE-INFO-G.
      *           �ی����i�ی��g�����j
                  06  MDCRES-INSURANCE-INFO     OCCURS  100 TIMES.
      *               �ی��g�����ԍ�
                      07  MDCRES-COMBINATION-NUMBER     PIC X(04).
      *               �ی��g�������S����
                      07  MDCRES-COMBINATION-RATE       PIC X(03).
      *               �ی��g���� �L���J�n��
                      07  MDCRES-COMB-START-DATE        PIC X(10).
      *               �ی��g���� �L���I����
                      07  MDCRES-COMB-END-DATE          PIC X(10).
      *
      *               �ی��̎��
                      07  MDCRES-MAIN-INSURANCE-CLASS   PIC X(03).
      *               �ی��̎�ޖ���
                      07  MDCRES-MAIN-INSURANCE-NAME    PIC X(20).
      *               �ی��Ҕԍ�
                      07  MDCRES-MAIN-INSURANCE-NUMBER   PIC X(08).
      *               �L��
                      07  MDCRES-MAIN-MARK              PIC X(80).
      *               �ԍ�
                      07  MDCRES-MAIN-NUMBER            PIC X(80).
      *               �p���敪
                      07  MDCRES-MAIN-CONTINUATION      PIC X(01).
      *               �⏕�敪
                      07  MDCRES-MAIN-ASSISTANCE        PIC X(01).
                      07  MDCRES-MAIN-ASSISTANCE-NAME   PIC X(20).
      *               �{�l�Ƒ��敪
                      07  MDCRES-MAIN-FAMILY            PIC X(01).
      *               �J�n��
                      07  MDCRES-MAIN-START-DATE        PIC X(10).
                      07  MDCRES-MAIN-END-DATE          PIC X(10).
      *               ������
                      07  MDCRES-PUBLIC-INSURANCE-INFO           
                                              OCCURS  4 TIMES.
      *                 ����̎��
                        09  MDCRES-INSURANCE-CLASS         PIC X(03).
      *                 ����̎�ޖ���
                        09  MDCRES-INSURANCE-NAME          PIC X(20).
      *                 ���S�Ҕԍ�
                        09  MDCRES-PROVIDER-NUMBER         PIC X(08).
      *                 �󋋎Ҕԍ�
                        09  MDCRES-RECIPIENT-NUMBER        PIC X(20).
      *                 �O���|���S���i���j
                        09  MDCRES-RATE-OUTPATIENT         PIC X(04).
      *                 �O���|�Œ�z
                        09  MDCRES-MONEY-OUTPATIENT        PIC X(06).
      *                 �J�n��
                        09  MDCRES-INSURANCE-START-DATE    PIC X(10).
                        09  MDCRES-INSURANCE-END-DATE      PIC X(10).
      * ���N�G�X�g���O�Q
      *       �ꊇ�ύX���e�i���N�G�X�g���O�Q�j
               05  MDCRES-CHENGE-INFO.
      *           �ύX�O�@�ی��g�����ԍ�
                  07  MDCRES-IN-COMBINATION-NUMBER    PIC X(04).
      *           �ύX��@�ی��g�����ԍ�
                  07  MDCRES-OT-COMBINATION-NUMBER    PIC X(04).
      *           �ύX����
                  07  MDCRES-START-DAY         PIC X(02).
                  07  MDCRES-END-DAY           PIC X(02).
      *           ���[�ꊇ�X�V����
                  07  MDCRES-INCOME-CHANGE     PIC X(03).
      *        �ύX��A���[���
               05  MDCRES-CD-INF-G.
                   07  MDCRES-CD-INF-TBL     OCCURS   100.
      *                �f�Ó��t
                       09  MDCRES-CD-PERFORM-DATE       PIC X(10).
      *                �`�[�ԍ�
                       09  MDCRES-CD-INVOICD-NUMBER    PIC  X(07).
      *                �ی��g�����ԍ�
                       09  MDCRES-CD-COMBINATION-NUMBER PIC X(04).
      *                ���[���S����
                       09  MDCRES-CD-RATE               PIC X(03).
      *                �`�[���s���t
                       09  MDCRES-CD-DENPPRTYMD         PIC X(10).
      *                �ύX�O�@�������z
                       09  MDCRES-MAE-AC-MONEY          PIC X(07).
      *                �ύX��@�������z
                       09  MDCRES-ATO-AC-MONEY          PIC X(07).
      *                �X�V�s�敪
                       09  MDCRES-DC-ERR                PIC X(06).
      *        �x�����b�Z�[�W
               05  MDCRES-WARING-G.
                   07  MDCRES-WARING-OCC       OCCURS  20.
                       09  MDCRES-WARNING              PIC X(03).
                       09  MDCRES-WARNIG-MESG          PIC X(200).
      *