      ************************************************************
      *    �ạ̃��ɣͣ�����           COPY (CPRLF002.INC)        *
      *    ������ۥ⥸�塼����                                  *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 03.05.00     NACL-¿��Ǽ  07/05/28  ���š����٥ơ��֥� 20 �� 30
      *                                     ������ᥳ����̾�� X(80)
      ************************************************************
      *01  LF02-REC.
      * [MML�إå�]
      * ���Լ�ID
           03  LF02-PTID1               PIC X(20).
      * ���Լ�ID�Υơ��֥�̾
           03  LF02-IDTYPE1             PIC X(08).
      * ������ �Ŀ�ID [ɬ��]
           03  LF02-CRE001-PERSONALID   PIC X(20).
      * ������ �Ŀͥե�͡��� [����դ�ɬ��]
           03  LF02-CRE001-FULLNAME     PIC X(100).
      * ������ �Ŀ��� [����դ�ɬ��]
           03  LF02-CRE001-FAMILYNAME   PIC X(50).
      * ������ �Ŀ�̾ [����դ�ɬ��]
           03  LF02-CRE001-GIVENNAME    PIC X(50).
      * ������ �ߥɥ�͡���
           03  LF02-CRE001-MIDDLENAME   PIC X(50).
      * ������ ���񤭤ʤ�
           03  LF02-CRE001-PREFIX       PIC X(50).
      * ������ �ذ�
           03  LF02-CRE001-DEGREE       PIC X(50).
      * ������ ���߾���̾
           03  LF02-CRE001-FACILITYNAME PIC X(50).
      * ������ ����ID
           03  LF02-CRE001-FACILITYID   PIC X(20).
      * ������ ���Ų�̾��
           03  LF02-CRE001-DPERTNAME    PIC X(50).
      * ������ ���Ų�ID
           03  LF02-CRE001-DPERTID      PIC X(20).
      * ������ ����
           03  LF02-CRE001-ADRFULLNAME  PIC X(400).
      * ������ ����μ���
           03  LF02-CRE001-ADRCLASS     PIC X(10).
      * ������ ��ƻ�ܸ�
           03  LF02-CRE001-ADRPREF      PIC X(20).
      * ������ �ԡ��衢��
           03  LF02-CRE001-ADRCITY      PIC X(20).
      * ������ Į��¼
           03  LF02-CRE001-ADRTOWN      PIC X(20).
      * ������ ���ϡ�������
           03  LF02-CRE001-ADRHOMENUM   PIC X(200).
      * ������ ͹���ֹ�
           03  LF02-CRE001-ZIP          PIC X(8).
      * ������ �񥳡���
           03  LF02-CRE001-COUNTRY      PIC X(10).
      * ������ �᡼�륢�ɥ쥹
           03  LF02-CRE001-EMAIL        PIC X(100).
      * ������ �Գ�����
           03  LF02-CRE001-PHONEAREA    PIC X(10).
      * ������ ����
           03  LF02-CRE001-PHONECITY    PIC X(10).
      * ������ �����ֹ�
           03  LF02-CRE001-PHONENUM     PIC X(10).
      * ������ �����ֹ�
           03  LF02-CRE001-PHONEEXT     PIC X(10).
      * ������ ���ֹ�
           03  LF02-CRE001-PHONECOUNTRY PIC X(10).
      * ������ ���ѻ����Ӥʤɤ򵭺�
           03  LF02-CRE001-PHONEMEMO    PIC X(100).
      * ������ ���֤μ��ॳ����
           03  LF02-CRE001-PHONEEQUIP   PIC X(10).
      * ������ �����Ԥλ�� [ɬ��]
           03  LF02-CRE001-LICENSE      PIC X(20).
      * [������ۥ⥸�塼��]
      * ʸ���ˡ���ID
           03  LF02-UID1                PIC X(36).
      * ������Ż���¸�γ�����
           03  LF02-CONFIRMDATE1        PIC X(10).
      * ������ �Ŀ�ID [ɬ��]
           03  LF02-CRE401-PERSONALID   PIC X(20).
      * ������ �Ŀͥե�͡��� [����դ�ɬ��]
           03  LF02-CRE401-FULLNAME     PIC X(100).
      * ������ �Ŀ��� [����դ�ɬ��]
           03  LF02-CRE401-FAMILYNAME   PIC X(50).
      * ������ �Ŀ�̾ [����դ�ɬ��]
           03  LF02-CRE401-GIVENNAME    PIC X(50).
      * ������ �ߥɥ�͡���
           03  LF02-CRE401-MIDDLENAME   PIC X(50).
      * ������ ���񤭤ʤ�
           03  LF02-CRE401-PREFIX       PIC X(50).
      * ������ �ذ�
           03  LF02-CRE401-DEGREE       PIC X(50).
      * ������ ���߾���̾
           03  LF02-CRE401-FACILITYNAME PIC X(50).
      * ������ ����ID
           03  LF02-CRE401-FACILITYID   PIC X(20).
      * ������ ���Ų�̾��
           03  LF02-CRE401-DPERTNAME    PIC X(50).
      * ������ ���Ų�ID
           03  LF02-CRE401-DPERTID      PIC X(20).
      * ������ ����
           03  LF02-CRE401-ADRFULLNAME  PIC X(400).
      * ������ ����μ���
           03  LF02-CRE401-ADRCLASS     PIC X(10).
      * ������ ��ƻ�ܸ�
           03  LF02-CRE401-ADRPREF      PIC X(20).
      * ������ �ԡ��衢��
           03  LF02-CRE401-ADRCITY      PIC X(20).
      * ������ Į��¼
           03  LF02-CRE401-ADRTOWN      PIC X(20).
      * ������ ���ϡ�������
           03  LF02-CRE401-ADRHOMENUM   PIC X(200).
      * ������ ͹���ֹ�
           03  LF02-CRE401-ZIP          PIC X(8).
      * ������ �񥳡���
           03  LF02-CRE401-COUNTRY      PIC X(10).
      * ������ �᡼�륢�ɥ쥹
           03  LF02-CRE401-EMAIL        PIC X(100).
      * ������ �Գ�����
           03  LF02-CRE401-PHONEAREA    PIC X(10).
      * ������ ����
           03  LF02-CRE401-PHONECITY    PIC X(10).
      * ������ �����ֹ�
           03  LF02-CRE401-PHONENUM     PIC X(10).
      * ������ �����ֹ�
           03  LF02-CRE401-PHONEEXT     PIC X(10).
      * ������ ���ֹ�
           03  LF02-CRE401-PHONECOUNTRY PIC X(10).
      * ������ ���ѻ����Ӥʤɤ򵭺�
           03  LF02-CRE401-PHONEMEMO    PIC X(100).
      * ������ ���֤μ��ॳ����
           03  LF02-CRE401-PHONEEQUIP   PIC X(10).
      * ������ �����Ԥλ�� [ɬ��]
           03  LF02-CRE401-LICENSE      PIC X(20).
      * ������ۥ⥸�塼��ξ���(claimA:status) (account������)
           03  LF02-STATUS              PIC X(07).
      *��������ȯ������
           03  LF02-ORDER-TIME          PIC X(19).
      *�»�ͽ������
           03  LF02-APPOINT-TIME        PIC X(19).
      *��������
           03  LF02-REGIST-TIME         PIC X(19).
      *�»�����
           03  LF02-PERFORM-TIME        PIC X(19).
      *��׽�λ����
           03  LF02-ACCOUNT-TIME        PIC X(19).
      *������ʬ (true OR false)
           03  LF02-NYUGAIKBN           PIC X(05).
      *���ֳ���ʬ
           03  LF02-JIKANNGAIKBN        PIC X(01).
      *�����ݸ��գɣ�
           03  LF02-HOKEN-UID           PIC X(36).
      *���̰�����ᥳ�����η�̾
           03  LF02-DEFAULT-TABLED      PIC X(100).
      *���Ų�̾��
           03  LF02-SRYKA-NAME          PIC X(12).
      * ���Ų�̾�Τ�ɽ��ˡ (mmlDp:repCode)
           03  LF02-SRYKA-REPCODE       PIC X(01).
      * ���Ų�ID
           03  LF02-SRYKA-ID            PIC X(02).
      * ���Ų�ID�μ��� (mmlCm:type)
           03  LF02-SRYKA-IDCODE        PIC X(07).
      * ��������̾��
           03  LF02-SNRBT-NAME          PIC X(12).
      * ��������̾�Τ�ɽ��ˡ (mmlDp:repCode)
           03  LF02-SNRBT-REPCODE       PIC X(01).
      * ��������ID
           03  LF02-SNRBT-ID            PIC X(02).
      * ��������ID�μ��� (mmlCm:type)
           03  LF02-SNRBT-IDCODE        PIC X(07).
      * ���ݸ�����
           03  LF02-HKNSBT              PIC X(20).
      * ���ݸ����̥�����
           03  LF02-HKNSBTCODE          PIC X(2).
      * ���ݸ����̥����ɥơ��֥�̾
           03  LF02-HKNSBTTBL           PIC X(7).
      *
           03  LF02-SRYTBL             OCCURS  30.
      *���Ź԰ٶ�ʬ������ (claimA:classCode)
               05  LF02-SRYCD          PIC  X(03).
      * ���Ź԰ٶ�ʬ�����ɤΥơ��֥�̾ (claimA:classCodeId)
               05  LF02-SRYCDTBL       PIC  X(08).
      *���Ź԰ٶ�ʬ̾
               05  LF02-SRYNAME        PIC  X(40).
      *�����������(Z�Խ�)
               05  LF02-TENNSUALL      PIC  X(16).
      *���������(Z�Խ�)
               05  LF02-GINGAKUALL     PIC  X(16).
      *��ˡ
               05  LF02-ADMINISTRATION
                                       PIC  X(50).
      *��ˡ������
               05  LF02-ADMINCODE      PIC  X(09).
      * ��ˡ�����ɤΥơ��֥�̾ (claimA:adminCodeId)
               05  LF02-ADMINCODETBL   PIC  X(08).
      *��ˡ������
               05  LF02-ADMMEMO        PIC  X(100).
      *�����������(Z�Խ�)
               05  LF02-KAISU          PIC  X(16).
      *�굻���������(Z�Խ�)
               05  LF02-SYUGI          PIC  X(16).
      *�������������(Z�Խ�)
               05  LF02-ZAIRYOU        PIC  X(16).
      *�������������(Z�Խ�)
               05  LF02-YAKUZAI        PIC  X(16).
      *����ʬ���ե饰(true OR false)
               05  LF02-PPSFLG         PIC  X(05).
      *���٥ơ��֥�
               05  LF02-SRYTBL       OCCURS  30.
      *���ż��̶�ʬ������(0:�굻��1:����,2:����)
                   07  LF02-SRYSYUBETSU
                                       PIC  X(01).
      * ���ż��̶�ʬ�����ɤΥơ��֥�̾ (claimA:subclassCodeId)
                   07  LF02-SRYSYUBETSUTBL
                                       PIC  X(08).
      *������ᥳ����(S + ���Ź԰٥�����)
                   07  LF02-IJICD      PIC  X(10).
      *������ᥳ�����η�̾
                   07  LF02-TBLID      PIC  X(20).
      *������ᥳ����
                   07  LF02-ALSCODE    PIC  X(10).
      *������ᥳ�����η�̾
                   07  LF02-ALSTBLID   PIC  X(20).
      *������ᥳ����̾��
      *@@@@@       07  LF02-ALSNAME    PIC  X(40).
                   07  LF02-ALSNAME    PIC  X(80).
      *����(Z�Խ�)
                   07  LF02-SURYO      PIC  X(12).
      *���̥�����
                   07  LF02-SURYOCD    PIC  X(02).
      * ���̥����ɤΥơ��֥�̾ (claimA:numberCodeId)
                   07  LF02-SURYOCDTBL PIC  X(08).
      *ñ��
                   07  LF02-TANI       PIC  X(20).
      *��������(Z�Խ�)
                   07  LF02-SEIKYUTEN  PIC  X(16).
      *����ñ��(Z�Խ�)
                   07  LF02-MEISAITANKA
                                       PIC  X(16).
      *���׻���(PTnHnM,5����23ʬ��PT5H23M)
                   07  LF02-DURATION   PIC  X(19).
      *����
                   07  LF02-LOCATION   PIC  X(50).
      *�ե����
                   07  LF02-FILM       OCCURS  1.
      *�ե���ॵ����
                     09  LF02-FILMSIZE PIC  X(50).
      *�ե���ॵ����������
                     09  LF02-FILMSIZECD
                                       PIC  X(02).
      * �ե���ॵ���������ɤΥơ��֥�̾ (claimA:numberCodeId)
                     09  LF02-FILMSIZECDTBL
                                       PIC  X(08).
      *�ե����ʬ��
                     09  LF02-FILMDEV  PIC  X(02).
      *�ե�������(Z�Խ�)
                     09  LF02-FILMNUM  PIC  X(06).
      *��Ϣ�굻
                   07  LF02-EVENT      PIC  X(50).
      *������
                   07  LF02-EVENTSTART PIC  X(10).
      *��λ��
                   07  LF02-EVENTEND   PIC  X(10).
      *���
                   07  LF02-SRYMEMO    PIC  X(100).
      *�����
               05  LF02-MEMO           PIC  X(1000).
      *    03  LF02-MEMO               PIC  X(100).