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
      *    ���Уɥ쥹�ݥ󥹣ạ̃��ɣͼ�����������(xml2)
      *                                      COPY (CPCLAIMFRONTV2RES.INC)
      ******************************************************************
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
       01  XML-CLAIM-FRONTV2RES.
           02  CLFRRES-CONTEXT           PIC S9(9)   BINARY.
           02  CLFRRES-OBJECT            PIC X(36).
           02  CLFRRES-MODE              PIC S9(9)   BINARY.
           02  CLFRRES-RECORDNAME        PIC X(128).
           02  CLFRRES-CLAIM-FRONT.
      *        �»���
               03  CLFRRES-INFORMATION-DATE    PIC X(10).
      *        �»ܻ���
               03  CLFRRES-INFORMATION-TIME    PIC X(8).
      *        �ֵѥ�����
               03  CLFRRES-API-RESULT          PIC X(2).
      *        �ֵѥ�å�����
               03  CLFRRES-API-RESULT-MSG      PIC X(100).
      *        �����ֹ�
               03  CLFRRES-PATIENTID           PIC X(20).
      *
      * �»���(CCYY-MM-DDTHH:MM:SS����)
               03  CLFRRES-INFORMATION-HEN     PIC X(19).
      * ���Լ�ID
               03  CLFRRES-PTID1               PIC X(20).
      * ID�μ��ॳ����
               03  CLFRRES-IDTYPE1             PIC X(08).
      * ������ �Ŀ�ID [ɬ��]
               03  CLFRRES-CRE001-PERSONALID   PIC X(20).
      * ������ �Ŀͥե�͡��� [����դ�ɬ��]
               03  CLFRRES-CRE001-FULLNAME     PIC X(100).
      * ������ �Ŀ��� [����դ�ɬ��]
               03  CLFRRES-CRE001-FAMILYNAME   PIC X(50).
      * ������ �Ŀ�̾ [����դ�ɬ��]
               03  CLFRRES-CRE001-GIVENNAME    PIC X(50).
      * ������ �ߥɥ�͡���
               03  CLFRRES-CRE001-MIDDLENAME   PIC X(50).
      * ������ ���񤭤ʤ�
               03  CLFRRES-CRE001-PREFIX       PIC X(50).
      * ������ �ذ�
               03  CLFRRES-CRE001-DEGREE       PIC X(50).
      * ������ ���߾���̾
               03  CLFRRES-CRE001-FACILITYNAME PIC X(50).
      * ������ ����ID
               03  CLFRRES-CRE001-FACILITYID   PIC X(20).
      * ������ ���Ų�̾��
               03  CLFRRES-CRE001-DPERTNAME    PIC X(50).
      * ������ ���Ų�ID
               03  CLFRRES-CRE001-DPERTID      PIC X(20).
      * ������ ���� [����դ�ɬ��]
               03  CLFRRES-CRE001-ADRFULLNAME  PIC X(400).
      * ������ ����μ���
               03  CLFRRES-CRE001-ADRCLASS     PIC X(10).
      * ������ ��ƻ�ܸ� [����դ�ɬ��]
               03  CLFRRES-CRE001-ADRPREF      PIC X(20).
      * ������ �ԡ��衢�� [����դ�ɬ��]
               03  CLFRRES-CRE001-ADRCITY      PIC X(20).
      * ������ Į��¼ [����դ�ɬ��]
               03  CLFRRES-CRE001-ADRTOWN      PIC X(20).
      * ������ ���ϡ������� [����դ�ɬ��]
               03  CLFRRES-CRE001-ADRHOMENUM   PIC X(200).
      * ������ ͹���ֹ�
               03  CLFRRES-CRE001-ZIP          PIC X(8).
      * ������ �񥳡���
               03  CLFRRES-CRE001-COUNTRY      PIC X(10).
      * ������ �᡼�륢�ɥ쥹
               03  CLFRRES-CRE001-EMAIL        PIC X(100).
      * ������ �Գ����� [ɬ��]
               03  CLFRRES-CRE001-PHONEAREA    PIC X(10).
      * ������ ���� [ɬ��]
               03  CLFRRES-CRE001-PHONECITY    PIC X(10).
      * ������ �����ֹ� [ɬ��]
               03  CLFRRES-CRE001-PHONENUM     PIC X(10).
      * ������ �����ֹ�
               03  CLFRRES-CRE001-PHONEEXT     PIC X(10).
      * ������ ���ֹ�
               03  CLFRRES-CRE001-PHONECOUNTRY PIC X(10).
      * ������ ���ѻ����Ӥʤɤ򵭺�
               03  CLFRRES-CRE001-PHONEMEMO    PIC X(100).
      * ������ ���֤μ��ॳ����
               03  CLFRRES-CRE001-PHONEEQUIP   PIC X(10).
      * ������ �����Ԥλ�� [ɬ��]
               03  CLFRRES-CRE001-LICENSE      PIC X(20).
      *##
      * ���󥹥������ (<extRefs />)
               03  CLFRRES-M-RX001-I1          PIC X(20).
      * ���󥹥������  (<accessRight permit="all" />) 
               03  CLFRRES-M-RI101-ACC1        PIC X(50).
      * ���󥹥������ (Data of Regist) 
               03  CLFRRES-M-RI101-TITLE       PIC X(20).
      *##
      *
      * [���Ծ���]
      * ʸ���ˡ���ID
               03  CLFRRES-UID1                PIC X(36).
      * ������Ż���¸������
               03  CLFRRES-CONFIRMDATE1        PIC X(10).
      * ������ �Ŀ�ID [ɬ��]
               03  CLFRRES-CRE101-PERSONALID   PIC X(20).
      * ������ �Ŀͥե�͡��� [����դ�ɬ��]
               03  CLFRRES-CRE101-FULLNAME     PIC X(100).
      * ������ �Ŀ��� [����դ�ɬ��]
               03  CLFRRES-CRE101-FAMILYNAME   PIC X(50).
      * ������ �Ŀ�̾ [����դ�ɬ��]
               03  CLFRRES-CRE101-GIVENNAME    PIC X(50).
      * ������ �ߥɥ�͡���
               03  CLFRRES-CRE101-MIDDLENAME   PIC X(50).
      * ������ ���񤭤ʤ�
               03  CLFRRES-CRE101-PREFIX       PIC X(50).
      * ������ �ذ�
               03  CLFRRES-CRE101-DEGREE       PIC X(50).
      * ������ ���߾���̾
               03  CLFRRES-CRE101-FACILITYNAME PIC X(50).
      * ������ ����ID
               03  CLFRRES-CRE101-FACILITYID   PIC X(20).
      * ������ ���Ų�̾��
               03  CLFRRES-CRE101-DPERTNAME    PIC X(50).
      * ������ ���Ų�ID
               03  CLFRRES-CRE101-DPERTID      PIC X(20).
      * ������ ���� [����դ�ɬ��]
               03  CLFRRES-CRE101-ADRFULLNAME  PIC X(400).
      * ������ ����μ���
               03  CLFRRES-CRE101-ADRCLASS     PIC X(10).
      * ������ ��ƻ�ܸ� [����դ�ɬ��]
               03  CLFRRES-CRE101-ADRPREF      PIC X(20).
      * ������ �ԡ��衢�� [����դ�ɬ��]
               03  CLFRRES-CRE101-ADRCITY      PIC X(20).
      * ������ Į��¼ [����դ�ɬ��]
               03  CLFRRES-CRE101-ADRTOWN      PIC X(20).
      * ������ ���ϡ������� [����դ�ɬ��]
               03  CLFRRES-CRE101-ADRHOMENUM   PIC X(200).
      * ������ ͹���ֹ�
               03  CLFRRES-CRE101-ZIP          PIC X(8).
      * ������ �񥳡���
               03  CLFRRES-CRE101-COUNTRY      PIC X(10).
      * ������ �᡼�륢�ɥ쥹
               03  CLFRRES-CRE101-EMAIL        PIC X(100).
      * ������ �Գ����� [ɬ��]
               03  CLFRRES-CRE101-PHONEAREA    PIC X(10).
      * ������ ���� [ɬ��]
               03  CLFRRES-CRE101-PHONECITY    PIC X(10).
      * ������ �����ֹ� [ɬ��]
               03  CLFRRES-CRE101-PHONENUM     PIC X(10).
      * ������ �����ֹ�
               03  CLFRRES-CRE101-PHONEEXT     PIC X(10).
      * ������ ���ֹ�
               03  CLFRRES-CRE101-PHONECOUNTRY PIC X(10).
      * ������ ���ѻ����Ӥʤɤ򵭺�
               03  CLFRRES-CRE101-PHONEMEMO    PIC X(100).
      * ������ ���֤μ��ॳ����
               03  CLFRRES-CRE101-PHONEEQUIP   PIC X(10).
      * ������ �����Ԥλ��
               03  CLFRRES-CRE101-LICENSE      PIC X(20).
      *##
      * ���󥹥������ (<extRefs />) 
               03  CLFRRES-M-RX101-I1          PIC X(20).
      *##
      * ���Լ�ID
               03  CLFRRES-PTID2               PIC X(20).
      * ���Լ�ID�Υơ��֥�̾
               03  CLFRRES-IDTYPE2             PIC X(08).
      * ��̾(����)��ɽ��ˡ
               03  CLFRRES-KANANAME-REPCODE    PIC X(01).
      * ��̾(����)�Υե�͡���
               03  CLFRRES-KANANAME            PIC X(100).
      * ��̾(����)��ɽ��ˡ
               03  CLFRRES-NAME-REPCODE        PIC X(01).
      * ��̾(����)�Υե�͡���
               03  CLFRRES-NAME                PIC X(100).
      * ��ǯ����
               03  CLFRRES-BIRTHDAY            PIC X(10).
      * ����
               03  CLFRRES-SEX                 PIC X(07).
      * ����(����)
               03  CLFRRES-HOME-ADRS           PIC X(400).
      * ����(����) ���ॳ����
               03  CLFRRES-HOME-KREPCODE       PIC X(09).
      * ����(����) ͹���ֹ�
               03  CLFRRES-HOME-POST           PIC X(08).
      * ����(Ϣ����)
               03  CLFRRES-RENRAKU-ADRS        PIC X(400).
      * ����(Ϣ����) ���ॳ����
               03  CLFRRES-RENRAKU-KREPCODE    PIC X(09).
      * ����(Ϣ����) ͹���ֹ�
               03  CLFRRES-RENRAKU-POST        PIC X(08).
      * ����(��̳��)
               03  CLFRRES-OFFICE-ADRS         PIC X(400).
      * ����(��̳��) ���ॳ����
               03  CLFRRES-OFFICE-KREPCODE     PIC X(09).
      * ����(��̳��) ͹���ֹ�
               03  CLFRRES-OFFICE-POST         PIC X(08).
      * ����(������)
               03  CLFRRES-KISEI-ADRS          PIC X(400).
      * ����(������) ���ॳ����
               03  CLFRRES-KISEI-KREPCODE      PIC X(09).
      * ����(������) ͹���ֹ�
               03  CLFRRES-KISEI-POST          PIC X(08).
      * �Գ�����
               03  CLFRRES-PHONE-AREA          PIC X(10).
      * ����
               03  CLFRRES-PHONE-CITY          PIC X(10).
      * �����ֹ�
               03  CLFRRES-PHONE-NUM           PIC X(10).
      * �����ֹ�
               03  CLFRRES-PHONE-EXT           PIC X(10).
      * ���ֹ�
               03  CLFRRES-PHONE-COUNTRY       PIC X(10).
      * ���ѻ����Ӥʤɤ򵭺�
               03  CLFRRES-PHONE-MEMO          PIC X(100).
      * ���֤μ��ॳ���� [��ά��ǽ��°��] (PH)
               03  CLFRRES-PHONE-EQUIP         PIC X(10).
      * ��˴�ե饰
               03  CLFRRES-DEATHKBN            PIC X(05).
      * [���ݸ�����]
               03  CLFRRES-TBL                OCCURS  20.
      *##
      * ���󥹥������ (<accessRight permit="all" />)
                   05  CLFRRES-M-RI2II-ACC1     PIC X(30).
      * ���󥹥������ (Data of Regist)
                   05  CLFRRES-M-RI2II-TITLE    PIC X(30).
      *##
      * ʸ���ˡ���ID
                   05  CLFRRES-UID2           PIC X(36).
      * ������Ż���¸�γ�����
                   05  CLFRRES-CONFIRMDATE2   PIC X(10).
      * ������ �Ŀ�ID [ɬ��]
                   05  CLFRRES-CRE201-PERSONALID   PIC X(20).
      * ������ �Ŀͥե�͡��� [����դ�ɬ��]
                   05  CLFRRES-CRE201-FULLNAME     PIC X(100).
      * ������ �Ŀ��� [����դ�ɬ��]
                   05  CLFRRES-CRE201-FAMILYNAME   PIC X(50).
      * ������ �Ŀ�̾ [����դ�ɬ��]
                   05  CLFRRES-CRE201-GIVENNAME    PIC X(50).
      * ������ �ߥɥ�͡���
                   05  CLFRRES-CRE201-MIDDLENAME   PIC X(50).
      * ������ ���񤭤ʤ�
                   05  CLFRRES-CRE201-PREFIX       PIC X(50).
      * ������ �ذ�
                   05  CLFRRES-CRE201-DEGREE       PIC X(50).
      * ������ ���߾���̾
                   05  CLFRRES-CRE201-FACILITYNAME PIC X(50).
      * ������ ����ID
                   05  CLFRRES-CRE201-FACILITYID   PIC X(20).
      * ������ ���Ų�̾��
                   05  CLFRRES-CRE201-DPERTNAME    PIC X(50).
      * ������ ���Ų�ID
                   05  CLFRRES-CRE201-DPERTID      PIC X(20).
      * ������ ���� [����դ�ɬ��]
                   05  CLFRRES-CRE201-ADRFULLNAME  PIC X(400).
      * ������ ����μ���
                   05  CLFRRES-CRE201-ADRCLASS     PIC X(10).
      * ������ ��ƻ�ܸ� [����դ�ɬ��]
                   05  CLFRRES-CRE201-ADRPREF      PIC X(20).
      * ������ �ԡ��衢�� [����դ�ɬ��]
                   05  CLFRRES-CRE201-ADRCITY      PIC X(20).
      * ������ Į��¼ [����դ�ɬ��]
                   05  CLFRRES-CRE201-ADRTOWN      PIC X(20).
      * ������ ���ϡ������� [����դ�ɬ��]
                   05  CLFRRES-CRE201-ADRHOMENUM   PIC X(200).
      * ������ ͹���ֹ�
                   05  CLFRRES-CRE201-ZIP          PIC X(8).
      * ������ �񥳡���
                   05  CLFRRES-CRE201-COUNTRY      PIC X(10).
      * ������ �᡼�륢�ɥ쥹
                   05  CLFRRES-CRE201-EMAIL        PIC X(100).
      * ������ �Գ����� [ɬ��]
                   05  CLFRRES-CRE201-PHONEAREA    PIC X(10).
      * ������ ���� [ɬ��]
                   05  CLFRRES-CRE201-PHONECITY    PIC X(10).
      * ������ �����ֹ� [ɬ��]
                   05  CLFRRES-CRE201-PHONENUM     PIC X(10).
      * ������ �����ֹ�
                   05  CLFRRES-CRE201-PHONEEXT     PIC X(10).
      * ������ ���ֹ�
                   05  CLFRRES-CRE201-PHONECOUNTRY PIC X(10).
      * ������ ���ѻ����Ӥʤɤ򵭺�
                   05  CLFRRES-CRE201-PHONEMEMO    PIC X(100).
      * ������ ���֤μ��ॳ����
                   05  CLFRRES-CRE201-PHONEEQUIP   PIC X(10).
      * ������ �����Ԥλ�� [ɬ��]
                   05  CLFRRES-CRE201-LICENSE      PIC X(20).
      *##
      * ���󥹥������  (<extRefs />)
                   05  CLFRRES-M-RX2II-I1     PIC X(20).
      *##
      * ���ݸ�����̾��
                   05  CLFRRES-HKNSBT              PIC X(20).
      * ���ݸ����̥�����
                   05  CLFRRES-HKNSBTCODE          PIC X(2).
      * ���ݸ����̥����ɥơ��֥�̾
                   05  CLFRRES-HKNSBTTBL           PIC X(7).
      *�ݸ����ֹ�
                   05  CLFRRES-HKNJANUM       PIC  X(08).
      *����
                   05  CLFRRES-KIGO           PIC  X(80).
      *�ֹ�
                   05  CLFRRES-NUM            PIC  X(80).
      *�ܿͲ�²��ʬ
                   05  CLFRRES-HONKZKKBN      PIC  X(05).
      *Ŭ�ѳ���ǯ����
                   05  CLFRRES-TEKSTYMD       PIC  X(10).
      *Ŭ�ѽ�λǯ����
                   05  CLFRRES-TEKEDYMD       PIC  X(10).
      * ����������ôΨ
                   05  CLFRRES-INRATIO        PIC X(5).
      * ���������ôΨ
                   05  CLFRRES-OUTRATIO       PIC X(5).
      *�������
                   05  CLFRRES-KOHINF       OCCURS  6.
      *ͥ����
                       07  CLFRRES-PRIORITY   PIC  X(01).
      *����̾��
                       07  CLFRRES-KOHINAME   PIC  X(50).
      *��ô���ֹ�
                       07  CLFRRES-FTNJANUM   PIC  X(08).
      *������ֹ�
                       07  CLFRRES-JKYSNUM    PIC  X(20).
      *Ŭ�ѳ���ǯ����
                       07  CLFRRES-TTEKSTYMD  PIC  X(10).
      *Ŭ�ѽ�λǯ����
                       07  CLFRRES-TTEKEDYMD  PIC  X(10).
      * ��ô��ޤ�����ôΨ
                       07  CLFRRES-RATIO-CODE PIC X(10).
                       07  CLFRRES-RATIO      PIC X(10).
      *##
      * ���󥹥������ (<accessRight permit="all" />)
               03  CLFRRES-M-RI301-ACC1     PIC X(50).
      * ���󥹥������ (Data of Regist) 
               03  CLFRRES-M-RI301-TITLE     PIC X(20).
      *##
      *[CLAIM]
      * ʸ���ˡ���ID
               03  CLFRRES-UID3                PIC X(36).
      * ������Ż���¸�γ�����
               03  CLFRRES-CONFIRMDATE3        PIC X(10).
      * ������ �Ŀ�ID [ɬ��]
               03  CLFRRES-CRE301-PERSONALID   PIC X(20).
      * ������ �Ŀͥե�͡��� [����դ�ɬ��]
               03  CLFRRES-CRE301-FULLNAME     PIC X(100).
      * ������ �Ŀ��� [����դ�ɬ��]
               03  CLFRRES-CRE301-FAMILYNAME   PIC X(50).
      * ������ �Ŀ�̾ [����դ�ɬ��]
               03  CLFRRES-CRE301-GIVENNAME    PIC X(50).
      * ������ �ߥɥ�͡���
               03  CLFRRES-CRE301-MIDDLENAME   PIC X(50).
      * ������ ���񤭤ʤ�
               03  CLFRRES-CRE301-PREFIX       PIC X(50).
      * ������ �ذ�
               03  CLFRRES-CRE301-DEGREE       PIC X(50).
      * ������ ���߾���̾
               03  CLFRRES-CRE301-FACILITYNAME PIC X(50).
      * ������ ����ID
               03  CLFRRES-CRE301-FACILITYID   PIC X(20).
      * ������ ���Ų�̾��
               03  CLFRRES-CRE301-DPERTNAME    PIC X(50).
      * ������ ���Ų�ID
               03  CLFRRES-CRE301-DPERTID      PIC X(20).
      * ������ ���� [����դ�ɬ��]
               03  CLFRRES-CRE301-ADRFULLNAME  PIC X(400).
      * ������ ����μ���
               03  CLFRRES-CRE301-ADRCLASS     PIC X(10).
      * ������ ��ƻ�ܸ� [����դ�ɬ��]
               03  CLFRRES-CRE301-ADRPREF      PIC X(20).
      * ������ �ԡ��衢�� [����դ�ɬ��]
               03  CLFRRES-CRE301-ADRCITY      PIC X(20).
      * ������ Į��¼ [����դ�ɬ��]
               03  CLFRRES-CRE301-ADRTOWN      PIC X(20).
      * ������ ���ϡ������� [����դ�ɬ��]
               03  CLFRRES-CRE301-ADRHOMENUM   PIC X(200).
      * ������ ͹���ֹ�
               03  CLFRRES-CRE301-ZIP          PIC X(8).
      * ������ �񥳡���
               03  CLFRRES-CRE301-COUNTRY      PIC X(10).
      * ������ �᡼�륢�ɥ쥹
               03  CLFRRES-CRE301-EMAIL        PIC X(100).
      * ������ �Գ����� [ɬ��]
               03  CLFRRES-CRE301-PHONEAREA    PIC X(10).
      * ������ ���� [ɬ��]
               03  CLFRRES-CRE301-PHONECITY    PIC X(10).
      * ������ �����ֹ� [ɬ��]
               03  CLFRRES-CRE301-PHONENUM     PIC X(10).
      * ������ �����ֹ�
               03  CLFRRES-CRE301-PHONEEXT     PIC X(10).
      * ������ ���ֹ�
               03  CLFRRES-CRE301-PHONECOUNTRY PIC X(10).
      * ������ ���ѻ����Ӥʤɤ򵭺�
               03  CLFRRES-CRE301-PHONEMEMO    PIC X(100).
      * ������ ���֤μ��ॳ����
               03  CLFRRES-CRE301-PHONEEQUIP   PIC X(10).
      * ������ �����Ԥλ�� [ɬ��]
               03  CLFRRES-CRE301-LICENSE      PIC X(20).
      *##
      * ���󥹥������ (<extRefs />)                  const
               03  CLFRRES-M-RX301-I1          PIC X(20).
      * ͽ������⥸�塼��ξ��� (claim:status)
               03  CLFRRES-UKEYMD-KREPCODE    PIC  X(07).
      * ��������ȯ������ (claim:oderTime)
               03  CLFRRES-CONFIRMDATE4       PIC  X(19).
      * �������� (claim:registTime)
               03  CLFRRES-UKEYMD             PIC  X(19).
      * ���������ʬ (claim:admitFlag)
               03  CLFRRES-NYUGAIKBN          PIC  X(05).
      * �����ݸ� (claim:insuranceUid)
               03  CLFRRES-FIL1               PIC  X(36).
      * ���̰�����ᥳ�����η�
               03  CLFRRES-IJISKYCOD          PIC  X(100).
      * ͽ��
               03  CLFRRES-YYK                PIC  X(20).
      * ͽ�󥳡��� (claim:appCode)
               03  CLFRRES-YYKCOD             PIC  X(14).
      *##
      * ���󥹥������ (claim:appCodeId="Claim009"")
               03  CLFRRES-C-B3                PIC X(30).
      * ͽ����
               03  CLFRRES-YYKMEMO            PIC  X(100).
      *���Ų�̾��
               03  CLFRRES-SRYNAME            PIC  X(12).
      *���Ų�ɽ��
               03  CLFRRES-SRYREP             PIC  X(1).
      * ���Ų�ID
               03  CLFRRES-SRYID              PIC  X(2).
      * ���Ų�ID�Υơ��֥�̾
               03  CLFRRES-SRYIDTBL           PIC  X(7).
      * ������Ų�̾��
               03  CLFRRES-SNRNAME            PIC  X(12).
      * ������Ų� ɽ��ˡ
               03  CLFRRES-SNRREP             PIC  X(1).
      * ������Ų�̾��
               03  CLFRRES-SNRID              PIC  X(2).
      * ������Ų� ID�μ��ॳ����
               03  CLFRRES-SNRIDTBL           PIC  X(7).
      * ���ż��̥����� (claim:classCode)
               03  CLFRRES-BUNDLE-CLASSCODE   PIC  X(3).
      *������ᥳ����  
               03  CLFRRES-IJICD             PIC  X(10).
      * ������ᥳ�����η�̾
               03  CLFRRES-ID                PIC  X(20).
      * �������̾��
               03  CLFRRES-HOSPSBT           PIC  X(40).
      * ���ե��
               03  CLFRRES-UKE-MEMO          PIC  X(1000).
      *