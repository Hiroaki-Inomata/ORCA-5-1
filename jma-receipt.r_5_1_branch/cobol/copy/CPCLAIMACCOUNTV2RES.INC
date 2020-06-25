      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      ******************************************************************
      *    ���Уɥ쥹�ݥ󥹣ạ̃��ɣ�������������(xml2)
      *                                      COPY (CPCLAIMACCOUNTV2RES.INC)
      ******************************************************************
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
       01  XML-CLAIM-ACCOUNTV2RES.
           02  CLACRES-CONTEXT           PIC S9(9)   BINARY.
           02  CLACRES-OBJECT            PIC X(36).
           02  CLACRES-MODE              PIC S9(9)   BINARY.
           02  CLACRES-RECORDNAME        PIC X(128).
           02  CLACRES-CLAIM-FRONT.
      *        �»���
               03  CLACRES-INFORMATION-DATE    PIC X(10).
      *        �»ܻ���
               03  CLACRES-INFORMATION-TIME    PIC X(8).
      *        �ֵѥ�����
               03  CLACRES-API-RESULT          PIC X(2).
      *        �ֵѥ�å�����
               03  CLACRES-API-RESULT-MSG      PIC X(100).
      *        �����ֹ�
               03  CLACRES-PATIENTID           PIC X(20).
      *
      * �»���(CCYY-MM-DDTHH:MM:SS����)
               03  CLACRES-INFORMATION-HEN     PIC X(19).
      * ���Լ�ID
               03  CLACRES-PTID1               PIC X(20).
      * ���Լ�ID�Υơ��֥�̾
               03  CLACRES-IDTYPE1        PIC X(08).
      * ������ �Ŀ�ID [ɬ��]
               03  CLACRES-CRE001-PERSONALID   PIC X(20).
      * ������ �Ŀͥե�͡��� [����դ�ɬ��]
               03  CLACRES-CRE001-FULLNAME     PIC X(100).
      * ������ �Ŀ��� [����դ�ɬ��]
               03  CLACRES-CRE001-FAMILYNAME   PIC X(50).
      * ������ �Ŀ�̾ [����դ�ɬ��]
               03  CLACRES-CRE001-GIVENNAME    PIC X(50).
      * ������ �ߥɥ�͡���
               03  CLACRES-CRE001-MIDDLENAME   PIC X(50).
      * ������ ���񤭤ʤ�
               03  CLACRES-CRE001-PREFIX       PIC X(50).
      * ������ �ذ�
               03  CLACRES-CRE001-DEGREE       PIC X(50).
      * ������ ���߾���̾
               03  CLACRES-CRE001-FACILITYNAME PIC X(50).
      * ������ ����ID
               03  CLACRES-CRE001-FACILITYID   PIC X(20).
      * ������ ���Ų�̾��
               03  CLACRES-CRE001-DPERTNAME    PIC X(50).
      * ������ ���Ų�ID
               03  CLACRES-CRE001-DPERTID      PIC X(20).
      * ������ ����
               03  CLACRES-CRE001-ADRFULLNAME  PIC X(400).
      * ������ ����μ���
               03  CLACRES-CRE001-ADRCLASS     PIC X(10).
      * ������ ��ƻ�ܸ�
               03  CLACRES-CRE001-ADRPREF      PIC X(20).
      * ������ �ԡ��衢��
               03  CLACRES-CRE001-ADRCITY      PIC X(20).
      * ������ Į��¼
               03  CLACRES-CRE001-ADRTOWN      PIC X(20).
      * ������ ���ϡ�������
               03  CLACRES-CRE001-ADRHOMENUM   PIC X(200).
      * ������ ͹���ֹ�
               03  CLACRES-CRE001-ZIP          PIC X(8).
      * ������ �񥳡���
               03  CLACRES-CRE001-COUNTRY      PIC X(10).
      * ������ �᡼�륢�ɥ쥹
               03  CLACRES-CRE001-EMAIL        PIC X(100).
      * ������ �Գ�����
               03  CLACRES-CRE001-PHONEAREA    PIC X(10).
      * ������ ����
               03  CLACRES-CRE001-PHONECITY    PIC X(10).
      * ������ �����ֹ�
               03  CLACRES-CRE001-PHONENUM     PIC X(10).
      * ������ �����ֹ�
               03  CLACRES-CRE001-PHONEEXT     PIC X(10).
      * ������ ���ֹ�
               03  CLACRES-CRE001-PHONECOUNTRY PIC X(10).
      * ������ ���ѻ����Ӥʤɤ򵭺�
               03  CLACRES-CRE001-PHONEMEMO    PIC X(100).
      * ������ ���֤μ��ॳ����
               03  CLACRES-CRE001-PHONEEQUIP   PIC X(10).
      * ������ �����Ԥλ�� [ɬ��]
               03  CLACRES-CRE001-LICENSE      PIC X(20).
      *## ���󥹥������ (<accessRight permit="all" />) const
               03  CLACRES-M-RI401-ACC1             PIC X(50).
      *## ���󥹥������ (Data of Amount) const
               03  CLACRES-M-RI401-TITLE            PIC X(20).
      *
      * [������ۥ⥸�塼��]
      * ʸ���ˡ���ID
               03  CLACRES-UID1                PIC X(36).
      * ������Ż���¸�γ�����
               03  CLACRES-CONFIRMDATE1        PIC X(10).
      * ������ �Ŀ�ID [ɬ��]
               03  CLACRES-CRE401-PERSONALID   PIC X(20).
      * ������ �Ŀͥե�͡��� [����դ�ɬ��]
               03  CLACRES-CRE401-FULLNAME     PIC X(100).
      * ������ �Ŀ��� [����դ�ɬ��]
               03  CLACRES-CRE401-FAMILYNAME   PIC X(50).
      * ������ �Ŀ�̾ [����դ�ɬ��]
               03  CLACRES-CRE401-GIVENNAME    PIC X(50).
      * ������ �ߥɥ�͡���
               03  CLACRES-CRE401-MIDDLENAME   PIC X(50).
      * ������ ���񤭤ʤ�
               03  CLACRES-CRE401-PREFIX       PIC X(50).
      * ������ �ذ�
               03  CLACRES-CRE401-DEGREE       PIC X(50).
      * ������ ���߾���̾
               03  CLACRES-CRE401-FACILITYNAME PIC X(50).
      * ������ ����ID
               03  CLACRES-CRE401-FACILITYID   PIC X(20).
      * ������ ���Ų�̾��
               03  CLACRES-CRE401-DPERTNAME    PIC X(50).
      * ������ ���Ų�ID
               03  CLACRES-CRE401-DPERTID      PIC X(20).
      * ������ ����
               03  CLACRES-CRE401-ADRFULLNAME  PIC X(400).
      * ������ ����μ���
               03  CLACRES-CRE401-ADRCLASS     PIC X(10).
      * ������ ��ƻ�ܸ�
               03  CLACRES-CRE401-ADRPREF      PIC X(20).
      * ������ �ԡ��衢��
               03  CLACRES-CRE401-ADRCITY      PIC X(20).
      * ������ Į��¼
               03  CLACRES-CRE401-ADRTOWN      PIC X(20).
      * ������ ���ϡ�������
               03  CLACRES-CRE401-ADRHOMENUM   PIC X(200).
      * ������ ͹���ֹ�
               03  CLACRES-CRE401-ZIP          PIC X(8).
      * ������ �񥳡���
               03  CLACRES-CRE401-COUNTRY      PIC X(10).
      * ������ �᡼�륢�ɥ쥹
               03  CLACRES-CRE401-EMAIL        PIC X(100).
      * ������ �Գ�����
               03  CLACRES-CRE401-PHONEAREA    PIC X(10).
      * ������ ����
               03  CLACRES-CRE401-PHONECITY    PIC X(10).
      * ������ �����ֹ�
               03  CLACRES-CRE401-PHONENUM     PIC X(10).
      * ������ �����ֹ�
               03  CLACRES-CRE401-PHONEEXT     PIC X(10).
      * ������ ���ֹ�
               03  CLACRES-CRE401-PHONECOUNTRY PIC X(10).
      * ������ ���ѻ����Ӥʤɤ򵭺�
               03  CLACRES-CRE401-PHONEMEMO    PIC X(100).
      * ������ ���֤μ��ॳ����
               03  CLACRES-CRE401-PHONEEQUIP   PIC X(10).
      * ������ �����Ԥλ�� [ɬ��]
               03  CLACRES-CRE401-LICENSE      PIC X(20).
      *### ���󥹥������ (<extRefs />)                const
               03  CLACRES-M-RX401-I1               PIC X(20).
      *
      * ������ۥ⥸�塼��ξ���(claimA:status) (account������)
               03  CLACRES-STATUS              PIC X(07).
      *��������ȯ������
               03  CLACRES-ORDER-TIME          PIC X(19).
      *�»�ͽ������
               03  CLACRES-APPOINT-TIME        PIC X(19).
      *��������
               03  CLACRES-REGIST-TIME         PIC X(19).
      *�»�����
               03  CLACRES-PERFORM-TIME        PIC X(19).
      *��׽�λ����
               03  CLACRES-ACCOUNT-TIME        PIC X(19).
      *������ʬ (true OR false)
               03  CLACRES-NYUGAIKBN           PIC X(05).
      *���ֳ���ʬ
               03  CLACRES-JIKANNGAIKBN        PIC X(01).
      *�����ݸ��գɣ�
               03  CLACRES-HOKEN-UID           PIC X(36).
      *���̰�����ᥳ�����η�̾
               03  CLACRES-DEFAULT-TABLED      PIC X(100).
      *���Ų�̾��
               03  CLACRES-SRYKA-NAME          PIC X(12).
      * ���Ų�̾�Τ�ɽ��ˡ (mmlDp:repCode)
               03  CLACRES-SRYKA-REPCODE       PIC X(01).
      * ���Ų�ID
               03  CLACRES-SRYKA-ID            PIC X(02).
      * ���Ų�ID�μ��� (mmlCm:type)
               03  CLACRES-SRYKA-IDCODE        PIC X(07).
      * ��������̾��
               03  CLACRES-SNRBT-NAME          PIC X(12).
      * ��������̾�Τ�ɽ��ˡ (mmlDp:repCode)
               03  CLACRES-SNRBT-REPCODE       PIC X(01).
      * ��������ID
               03  CLACRES-SNRBT-ID            PIC X(02).
      * ��������ID�μ��� (mmlCm:type)
               03  CLACRES-SNRBT-IDCODE        PIC X(07).
      * ���ݸ�����
               03  CLACRES-HKNSBT              PIC X(20).
      * ���ݸ����̥�����
               03  CLACRES-HKNSBTCODE          PIC X(2).
      * ���ݸ����̥����ɥơ��֥�̾
               03  CLACRES-HKNSBTTBL           PIC X(7).
      *
               03  CLACRES-SRYTBL             OCCURS  30.
      *���Ź԰ٶ�ʬ������ (claimA:classCode)
                   05  CLACRES-SRYCD          PIC  X(03).
      * ���Ź԰ٶ�ʬ�����ɤΥơ��֥�̾ (claimA:classCodeId)
                   05  CLACRES-SRYCDTBL       PIC  X(08).
      *���Ź԰ٶ�ʬ̾
                   05  CLACRES-SRYNAME        PIC  X(40).
      *�����������(Z�Խ�)
                   05  CLACRES-TENNSUALL      PIC  X(16).
      *���������(Z�Խ�)
                   05  CLACRES-GINGAKUALL     PIC  X(16).
      *��ˡ
                   05  CLACRES-ADMINISTRATION
                                       PIC  X(50).
      *��ˡ������
                   05  CLACRES-ADMINCODE      PIC  X(09).
      * ��ˡ�����ɤΥơ��֥�̾ (claimA:adminCodeId)
                   05  CLACRES-ADMINCODETBL   PIC  X(08).
      *��ˡ������
                   05  CLACRES-ADMMEMO        PIC  X(100).
      *�����������(Z�Խ�)
                   05  CLACRES-KAISU          PIC  X(16).
      *�굻���������(Z�Խ�)
                   05  CLACRES-SYUGI          PIC  X(16).
      *�������������(Z�Խ�)
                   05  CLACRES-ZAIRYOU        PIC  X(16).
      *�������������(Z�Խ�)
                   05  CLACRES-YAKUZAI        PIC  X(16).
      *����ʬ���ե饰(true OR false)
                   05  CLACRES-PPSFLG         PIC  X(05).
      *���٥ơ��֥�
                   05  CLACRES-SRYTBL       OCCURS  30.
      *���ż��̶�ʬ������(0:�굻��1:����,2:����)
                       07  CLACRES-SRYSYUBETSU
                                       PIC  X(01).
      * ���ż��̶�ʬ�����ɤΥơ��֥�̾ (claimA:subclassCodeId)
                       07  CLACRES-SRYSYUBETSUTBL
                                       PIC  X(08).
      *������ᥳ����(S + ���Ź԰٥�����)
                       07  CLACRES-IJICD      PIC  X(10).
      *������ᥳ�����η�̾
                       07  CLACRES-TBLID      PIC  X(20).
      *������ᥳ����
                       07  CLACRES-ALSCODE    PIC  X(10).
      *������ᥳ�����η�̾
                       07  CLACRES-ALSTBLID   PIC  X(20).
      *������ᥳ����̾��
                       07  CLACRES-ALSNAME    PIC  X(80).
      *����(Z�Խ�)
                       07  CLACRES-SURYO      PIC  X(12).
      *���̥�����
                       07  CLACRES-SURYOCD    PIC  X(02).
      * ���̥����ɤΥơ��֥�̾ (claimA:numberCodeId)
                       07  CLACRES-SURYOCDTBL PIC  X(08).
      *ñ��
                       07  CLACRES-TANI       PIC  X(20).
      *��������(Z�Խ�)
                       07  CLACRES-SEIKYUTEN  PIC  X(16).
      *����ñ��(Z�Խ�)
                       07  CLACRES-MEISAITANKA
                                       PIC  X(16).
      *���׻���(PTnHnM,5����23ʬ��PT5H23M)
                       07  CLACRES-DURATION   PIC  X(19).
      *����
                       07  CLACRES-LOCATION   PIC  X(50).
      *�ե����
      *�ե���ॵ����
                       07  CLACRES-FILMSIZE   PIC  X(50).
      *�ե���ॵ����������
                       07  CLACRES-FILMSIZECD
                                              PIC  X(02).
      * �ե���ॵ���������ɤΥơ��֥�̾ (claimA:numberCodeId)
                       07  CLACRES-FILMSIZECDTBL
                                              PIC  X(08).
      *�ե����ʬ��
                       07  CLACRES-FILMDEV    PIC  X(02).
      *�ե�������(Z�Խ�)
                       07  CLACRES-FILMNUM    PIC  X(06).
      *��Ϣ�굻
                       07  CLACRES-EVENT      PIC  X(50).
      *������
                       07  CLACRES-EVENTSTART PIC  X(10).
      *��λ��
                       07  CLACRES-EVENTEND   PIC  X(10).
      *���
                       07  CLACRES-SRYMEMO    PIC  X(100).
      *�����
                   05  CLACRES-MEMO           PIC  X(1000).
