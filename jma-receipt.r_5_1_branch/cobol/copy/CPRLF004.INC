      ************************************************************
      *    �ạ̃��ɣͣ�����           COPY (CPRLF004.INC)        *
      *    �����⥸�塼����                                      *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-ƣ��    03/09/30  ��������⥸�塼����ѹ�
      *  02.05.00    NACL-����    04/10/20  �ɥ������������ɲ�
      *  04.07.00    NACL-¿��Ǽ  11/10/XX  UID�ɲ�
      *  04.07.00    NACL-¿��Ǽ  11/11/02  Ʊ����������ʬ�ɲ�
      *  04.07.00    NACL-¿��Ǽ  13/08/XX  API��̾��­�������ɲ�
      *  04.08.00    NACL-¿��Ǽ  17/03/XX  API�����������ʬ�ɲ�
      *  05.00.00    NACL-¿��Ǽ  20/03/XX  �ң���ǯ��������б�
      ************************************************************
      *01  LF04-REC.
      *
      *     03  LF04-SRYCD               PIC X(02).
      *
      *
      * [MML�إå�]
      * ���Լ�ID
           03  LF04-PTID1               PIC X(20).
      * ���Լ�ID�Υơ��֥�̾
           03  LF04-IDTYPE1             PIC X(08).
      * ������ �Ŀ�ID [ɬ��]
           03  LF04-CRE001-PERSONALID   PIC X(20).
      * ������ �Ŀͥե�͡��� [����դ�ɬ��]
           03  LF04-CRE001-FULLNAME     PIC X(100).
      * ������ �Ŀ��� [����դ�ɬ��]
           03  LF04-CRE001-FAMILYNAME   PIC X(50).
      * ������ �Ŀ�̾ [����դ�ɬ��]
           03  LF04-CRE001-GIVENNAME    PIC X(50).
      * ������ �ߥɥ�͡���
           03  LF04-CRE001-MIDDLENAME   PIC X(50).
      * ������ ���񤭤ʤ�
           03  LF04-CRE001-PREFIX       PIC X(50).
      * ������ �ذ�
           03  LF04-CRE001-DEGREE       PIC X(50).
      * ������ ���߾���̾
           03  LF04-CRE001-FACILITYNAME PIC X(50).
      * ������ ����ID
           03  LF04-CRE001-FACILITYID   PIC X(20).
      * ������ ���Ų�̾��
           03  LF04-CRE001-DPERTNAME    PIC X(50).
      * ������ ���Ų�ID
           03  LF04-CRE001-DPERTID      PIC X(20).
      * ������ ����
           03  LF04-CRE001-ADRFULLNAME  PIC X(400).
      * ������ ����μ���
           03  LF04-CRE001-ADRCLASS     PIC X(10).
      * ������ ��ƻ�ܸ�
           03  LF04-CRE001-ADRPREF      PIC X(20).
      * ������ �ԡ��衢��
           03  LF04-CRE001-ADRCITY      PIC X(20).
      * ������ Į��¼
           03  LF04-CRE001-ADRTOWN      PIC X(20).
      * ������ ���ϡ�������
           03  LF04-CRE001-ADRHOMENUM   PIC X(200).
      * ������ ͹���ֹ�
           03  LF04-CRE001-ZIP          PIC X(8).
      * ������ �񥳡���
           03  LF04-CRE001-COUNTRY      PIC X(10).
      * ������ �᡼�륢�ɥ쥹
           03  LF04-CRE001-EMAIL        PIC X(100).
      * ������ �Գ�����
           03  LF04-CRE001-PHONEAREA    PIC X(10).
      * ������ ����
           03  LF04-CRE001-PHONECITY    PIC X(10).
      * ������ �����ֹ�
           03  LF04-CRE001-PHONENUM     PIC X(10).
      * ������ �����ֹ�
           03  LF04-CRE001-PHONEEXT     PIC X(10).
      * ������ ���ֹ�
           03  LF04-CRE001-PHONECOUNTRY PIC X(10).
      * ������ ���ѻ����Ӥʤɤ򵭺�
           03  LF04-CRE001-PHONEMEMO    PIC X(100).
      * ������ ���֤μ��ॳ����
           03  LF04-CRE001-PHONEEQUIP   PIC X(10).
      * ������ �����Ԥλ�� [ɬ��]
           03  LF04-CRE001-LICENSE      PIC X(20).
      *
      *
      * [���Ծ���]
      * ʸ���ˡ���ID
           03  LF04-UID1                PIC X(36).
      * ������Ż���¸�γ�����
           03  LF04-CONFIRMDATE1        PIC X(10).
      * <ID����>
      * (���Լ�ID)
      * ���Լ�ID
           03  LF04-PTID2               PIC X(20).
      * ID�μ��ॳ���� (mmlCm:type)
           03  LF04-PTID2-TYPE          PIC X(8).
      * ID�μ��ॳ���ɤΥơ��֥�̾ (mmlCm:tableId) (MML0024)
           03  LF04-PTID2-TYPETBL       PIC X(7).
      * �����å��ǥ����å��� (mmlCm:checkDigit)
           03  LF04-PTID2-CD            PIC X(2).
      * �����å��ǥ����å����� (mmlCm:checkDigitSchema)
           03  LF04-PTID2-CDSCHEMA      PIC X(3).
      * (����¾��ID)
      * ID�μ��ॳ���� (mmlPi:type)
           03  LF04-OTHERID-TYPE        PIC X(20).
      * ����¾��ID
           03  LF04-OTHERID1            PIC X(20).
      * ID�μ��ॳ���� (mmlCm:type)
           03  LF04-OTHERID1-TYPE       PIC X(8).
      * ID�μ��ॳ���ɤΥơ��֥�̾ (mmlCm:tableId)
           03  LF04-OTHERID1-TYPETBL    PIC X(7).
      * �����å��ǥ����å��� (mmlCm:checkDigit)
           03  LF04-OTHERID1-CD         PIC X(2).
      * �����å��ǥ����å����� (mmlCm:checkDigitSchema)
           03  LF04-OTHERID1-CDSHEMA    PIC X(3).
      * <��̾����>
      * ɽ��ˡ (mmlNm:repCode)
           03  LF04-PATNAME-REPCODE     PIC X(1).
      * ɽ��ˡ�Υơ��֥�̾ (mmlNm:tableId) (MML0025)
           03  LF04-PATNAME-TBL         PIC X(7).
      * ��
           03  LF04-PATNAME-FAMILY      PIC X(50).
      * ̾
           03  LF04-PATNAME-GIVEN       PIC X(50).
      * �ߥɥ�͡���
           03  LF04-PATNAME-MIDDLE      PIC X(50).
      * �ե�͡���
           03  LF04-PATNAME-FULLNAME    PIC X(100).
      * ���񤭤ʤ�
           03  LF04-PATNAME-PREFIX      PIC X(50).
      * �ذ�
           03  LF04-PATNAME-DEGREE      PIC X(50).
      * <����¾���Ծ��� 1>
      * ��ǯ����
           03  LF04-PATNAME-BIRTHDAY    PIC X(10).
      * ����
           03  LF04-PATNAME-SEX         PIC X(7).
      * ���ҥ�����
           03  LF04-PATNAME-NATIONALITY PIC X(10).
      * �裲���ҥ����� (mmlPi:subtype)
           03  LF04-PATNAME-SUBNATIONAL PIC X(10).
      * ��������
           03  LF04-PATNAME-MARITAL     PIC X(10).
      * <�������>
           03  LF04-PATADR-TBL          OCCURS  4.
      * ɽ��ˡ (mmlAd:repCode)
               05  LF04-PATADR-REPCODE  PIC  X(1).
      * ɽ��ˡ�Υơ��֥�̾ (mmlAd:tableId)
               05  LF04-PATADR-TABLEID  PIC  X(7).
      * ����μ��ॳ���� (mmlAd:addressClass)
               05  LF04-PATADR-ADRCLASS PIC  X(10).
      * ��ƻ�ܸ�
               05  LF04-PATADR-PREF     PIC  X(20).
      * �ԡ��衢��
               05  LF04-PATADR-CITY     PIC  X(20).
      * Į��¼
               05  LF04-PATADR-TOWN     PIC  X(20).
      * ���ϡ����ܡ�����¾
               05  LF04-PATADR-HOMENUM  PIC  X(200).
      * ��Ϣ����
               05  LF04-PATADR-FULLNAME PIC  X(400).
      * ͹���ֹ�
               05  LF04-PATADR-ZIP      PIC  X(8).
      * �񥳡���
               05  LF04-PATADR-COUNTRY  PIC  X(10).
      * <�Żҥ᡼�륢�ɥ쥹>
      * �Żҥ᡼�륢�ɥ쥹
           03  LF04-PAT-EMAIL           PIC X(100).
      * <�����ֹ�>
      * ���֤μ��ॳ���� (mmlPh:telEquipType)
           03  LF04-PAT-PHONEEQUIP      PIC X(10).
      * �Գ�����
           03  LF04-PAT-PHONEAREA       PIC X(10).
      * ����
           03  LF04-PAT-PHONECITY       PIC X(10).
      * �����ֹ�
           03  LF04-PAT-PHONENUM        PIC X(10).
      * �����ֹ�
           03  LF04-PAT-PHONEEXT        PIC X(10).
      * ���ֹ�
           03  LF04-PAT-PHONECOUNTRY    PIC X(10).
      * ���ѻ����ӤʤɤΥ��
           03  LF04-PAT-PHONEMEMO       PIC X(100).
      * <����¾���Ծ��� 2>
      * ����ֹ�
           03  LF04-PAT-ACCOUNTNUM      PIC X(50).
      * �Ҳ��ֹ�
           03  LF04-PAT-SOCIALNUM       PIC X(50).
      * ��˴�ե饰
           03  LF04-PAT-DEATHFLG        PIC X(5).
      * ��˴�������� (mmlPi:date)
           03  LF04-PAT-DEATHDATE       PIC X(16).
      *
      *
      * [���ݸ��⥸�塼��]
           03  LF04-HKNTBL              OCCURS  2.
      * ʸ���ˡ���ID
               05  LF04-UID2                PIC X(36).
      * ������Ż���¸�γ�����
               05  LF04-CONFIRMDATE2        PIC X(10).
      * �񥿥��� (mmlHi:countryType)
               05  LF04-HKN-COUNTRYTYPE     PIC X(10).
      * ���ݸ�����
               05  LF04-HKN-HKNSBT          PIC X(20).
      * �ݸ����̥����� (mmlHi:ClassCode)
               05  LF04-HKN-HKNSBTCODE      PIC X(2).
      * �ݸ����̥����ɤΥơ��֥�̾ (mmlHi:tableId)
               05  LF04-HKN-HKNSBTTBL       PIC X(7).
      * ���ݸ��ֹ�
               05  LF04-HKN-HKNJANUM        PIC  X(08).
      * ���ݸ��Ե���
               05  LF04-HKN-KIGO            PIC  X(80).
      * ���ݸ����ֹ�
               05  LF04-HKN-NUM             PIC  X(80).
      * �ܿͲ�²��ʬ
               05  LF04-HKN-HONKZKKBN       PIC  X(05).
      * <���ݸ��Ծ���>
      * ɽ��ˡ (mmlNm:repCode)
               05  LF04-HIHKNNAME-REPCODE   PIC X(1).
      * ɽ��ˡ�Υơ��֥�̾ (mmlNm:tableId) (MML0025)
               05  LF04-HIHKNNAME-TBL       PIC X(7).
      * ��
               05  LF04-HIHKNNAME-FAMILY    PIC X(50).
      * ̾
               05  LF04-HIHKNNAME-GIVEN     PIC X(50).
      * �ߥɥ�͡���
               05  LF04-HIHKNNAME-MIDDLE    PIC X(50).
      * �ե�͡���
               05  LF04-HIHKNNAME-FULLNAME  PIC X(100).
      * ���񤭤ʤ�
               05  LF04-HIHKNNAME-PREFIX    PIC X(50).
      * �ذ�
               05  LF04-HIHKNNAME-DEGREE    PIC X(50).
      * <���ݸ��Խ���>
      * ɽ��ˡ (mmlAd:repCode)
               05  LF04-HIHKNADR-REPCODE    PIC  X(1).
      * ɽ��ˡ�Υơ��֥�̾ (mmlAd:tableId)
               05  LF04-HIHKNADR-TABLEID    PIC  X(7).
      * ����μ��ॳ���� (mmlAd:addressClass)
               05  LF04-HIHKNADR-ADRCLASS   PIC  X(10).
      * ��ƻ�ܸ�
               05  LF04-HIHKNADR-PREF       PIC  X(20).
      * �ԡ��衢��
               05  LF04-HIHKNADR-CITY       PIC  X(20).
      * Į��¼
               05  LF04-HIHKNADR-TOWN       PIC  X(20).
      * ���ϡ����ܡ�����¾
               05  LF04-HIHKNADR-HOMENUM    PIC  X(200).
      * ��Ϣ����
               05  LF04-HIHKNADR-FULLNAME   PIC  X(400).
      * ͹���ֹ�
               05  LF04-HIHKNADR-ZIP        PIC  X(8).
      * �񥳡���
               05  LF04-HIHKNADR-COUNTRY    PIC  X(10).
      * <���ݸ��������ֹ�>
      * ���֤μ��ॳ���� (mmltelEquipType)
               05  LF04-HIHKNPHONE-EQUIP    PIC X(10).
      * �Գ�����
               05  LF04-HIHKNPHONE-AREA     PIC X(10).
      * ����
               05  LF04-HIHKNPHONE-CITY     PIC X(10).
      * �����ֹ�
               05  LF04-HIHKNPHONE-NUM      PIC X(10).
      * �����ֹ�
               05  LF04-HIHKNPHONE-EXT      PIC X(10).
      * ���ֹ�
               05  LF04-HIHKNPHONE-COUNTRY  PIC X(10).
      * ���ѻ����ӤʤɤΥ��
               05  LF04-HIHKNPHONE-MEMO     PIC X(100).
      * <����¾���ݸ�����>
      * ����¾��³Ŭ������̾
               05  LF04-HKN-OTHERKEIZOKU    PIC  X(50).
      * Ŭ�ѳ���ǯ������������ (����ǯ����)
               05  LF04-HKN-TEKSTYMD        PIC  X(10).
      * Ŭ�ѽ�λǯ������ͭ������
               05  LF04-HKN-TEKEDYMD        PIC  X(10).
      * ����������ôΨ
               05  LF04-HKN-NYUINFUTAN      PIC  X(10).
      * ���������ôΨ
               05  LF04-HKN-GAIRAIFUTAN     PIC  X(10).
      * <�ݸ��Ի��߾���>
      * ɽ��ˡ (mmlFc:repCode)
               05  LF04-HKNSYASIS-REPCODE   PIC X(01).
      * ɽ��ˡ�Υơ��֥�̾ (mmlFc:tableId)
               05  LF04-HKNSYASIS-REPCODETBL
                                            PIC X(07).
      * ����̾��
               05  LF04-HKNSYASIS-NAME      PIC X(50).
      * ����ID
               05  LF04-HKNSYASIS-ID        PIC X(20).
      * ID�μ��ॳ����(mmlCm:type)
               05  LF04-HKNSYASIS-IDCODE    PIC X(10).
      * ID�μ��ॳ���ɤΥơ��֥�̾ (mmlCm:tableId) (MML0027)
               05  LF04-HKNSYASIS-IDCODETBL PIC X(07).
      * �����å��ǥ����å��� (mmlCm:checkDigit)
               05  LF04-HKNSYASIS-CD        PIC X(02).
      * �����å��ǥ����å����� (mmlCm:checkDigitSchema)
               05  LF04-HKNSYASIS-CDSCHEMA  PIC X(03).
      * <�ݸ��Խ���>
      * ɽ��ˡ (mmlAd:repCode)
               05  LF04-HKNSYAADR-REPCODE   PIC  X(1).
      * ɽ��ˡ�Υơ��֥�̾ (mmlAd:tableId)
               05  LF04-HKNSYAADR-TABLEID   PIC  X(7).
      * ����μ��ॳ���� (mmlAd:addressClass)
               05  LF04-HKNSYAADR-ADRCLASS  PIC  X(10).
      * ��ƻ�ܸ�
               05  LF04-HKNSYAADR-PREF      PIC  X(20).
      * �ԡ��衢��
               05  LF04-HKNSYAADR-CITY      PIC  X(20).
      * Į��¼
               05  LF04-HKNSYAADR-TOWN      PIC  X(20).
      * ���ϡ����ܡ�����¾
               05  LF04-HKNSYAADR-HOMENUM   PIC  X(200).
      * ��Ϣ����
               05  LF04-HKNSYAADR-FULLNAME  PIC  X(400).
      * ͹���ֹ�
               05  LF04-HKNSYAADR-ZIP       PIC  X(8).
      * �񥳡���
               05  LF04-HKNSYAADR-COUNTRY   PIC  X(10).
      * <�ݸ��������ֹ�>
      * ���֤μ��ॳ���� (mmltelEquipType)
               05  LF04-HKNSYAPHONE-EQUIP   PIC X(10).
      * �Գ�����
               05  LF04-HKNSYAPHONE-AREA    PIC X(10).
      * ����
               05  LF04-HKNSYAPHONE-CITY    PIC X(10).
      * �����ֹ�
               05  LF04-HKNSYAPHONE-NUM     PIC X(10).
      * �����ֹ�
               05  LF04-HKNSYAPHONE-EXT     PIC X(10).
      * ���ֹ�
               05  LF04-HKNSYAPHONE-COUNTRY PIC X(10).
      * ���ѻ����ӤʤɤΥ��
               05  LF04-HKNSYAPHONE-MEMO    PIC X(100).
      * <���Ƚ���߾���>
      * ɽ��ˡ (mmlFc:repCode)
               05  LF04-JGYSYASIS-REPCODE   PIC X(01).
      * ɽ��ˡ�Υơ��֥�̾ (mmlFc:tableId)
               05  LF04-JGYSYASIS-REPCODETBL
                                            PIC X(07).
      * ����̾��
               05  LF04-JGYSYASIS-NAME      PIC X(50).
      * ����ID
               05  LF04-JGYSYASIS-ID        PIC X(20).
      * ID�μ��ॳ����(mmlCm:type)
               05  LF04-JGYSYASIS-IDCODE    PIC X(10).
      * ID�μ��ॳ���ɤΥơ��֥�̾ (mmlCm:tableId) (MML0027)
               05  LF04-JGYSYASIS-IDCODETBL PIC X(07).
      * �����å��ǥ����å��� (mmlCm:checkDigit)
               05  LF04-JGYSYASIS-CD        PIC X(02).
      * �����å��ǥ����å����� (mmlCm:checkDigitSchema)
               05  LF04-JGYSYASIS-CDSCHEMA  PIC X(03).
      * <���Ƚ꽻��>
      * ɽ��ˡ (mmlAd:repCode)
               05  LF04-JGYSYAADR-REPCODE   PIC  X(1).
      * ɽ��ˡ�Υơ��֥�̾ (mmlAd:tableId)
               05  LF04-JGYSYAADR-TABLEID   PIC  X(7).
      * ����μ��ॳ���� (mmlAd:addressClass)
               05  LF04-JGYSYAADR-ADRCLASS  PIC  X(10).
      * ��ƻ�ܸ�
               05  LF04-JGYSYAADR-PREF      PIC  X(20).
      * �ԡ��衢��
               05  LF04-JGYSYAADR-CITY      PIC  X(20).
      * Į��¼
               05  LF04-JGYSYAADR-TOWN      PIC  X(20).
      * ���ϡ����ܡ�����¾
               05  LF04-JGYSYAADR-HOMENUM   PIC  X(200).
      * ��Ϣ����
               05  LF04-JGYSYAADR-FULLNAME  PIC  X(400).
      * ͹���ֹ�
               05  LF04-JGYSYAADR-ZIP       PIC  X(8).
      * �񥳡���
               05  LF04-JGYSYAADR-COUNTRY   PIC  X(10).
      * <���Ƚ������ֹ�>
      * ���֤μ��ॳ���� (mmltelEquipType)
               05  LF04-JGYSYAPHONE-EQUIP   PIC X(10).
      * �Գ�����
               05  LF04-JGYSYAPHONE-AREA    PIC X(10).
      * ����
               05  LF04-JGYSYAPHONE-CITY    PIC X(10).
      * �����ֹ�
               05  LF04-JGYSYAPHONE-NUM     PIC X(10).
      * �����ֹ�
               05  LF04-JGYSYAPHONE-EXT     PIC X(10).
      * ���ֹ�
               05  LF04-JGYSYAPHONE-COUNTRY PIC X(10).
      * ���ѻ����ӤʤɤΥ��
               05  LF04-JGYSYAPHONE-MEMO    PIC X(100).
      * <������ô���ž���>
               05  LF04-KOHINF       OCCURS  6.
      * ʣ�������ͥ���� (mmlHi:priority)
                   07  LF04-HKNKOH-PRIORITY     PIC  X(01).
      * ������ô̾��
                   07  LF04-HKNKOH-PROVIDER     PIC  X(20).
      * ��ô���ֹ�
                   07  LF04-HKNKOH-FTNJANUM     PIC  X(08).
      * ������ֹ�
                   07  LF04-HKNKOH-JKYSNUM      PIC  X(20).
      * Ŭ�ѳ���ǯ������������
                   07  LF04-HKNKOH-TTEKSTYMD    PIC  X(10).
      * Ŭ�ѽ�λǯ������ͭ������
                   07  LF04-HKNKOH-TTEKEDYMD    PIC  X(10).
      * ��ôΨ�ޤ�����ô�� (mmlHi:ratioType)
                   07  LF04-HKNKOH-RATIOTYPE    PIC  X(05).
      * ��ôΨ�ޤ�����ô��
                   07  LF04-HKNKOH-RATIO        PIC  X(10).
      *
      *
      * [��������⥸�塼��]
           03  LF04-SNDRRKTBL           OCCURS  50.
      * ʸ���ˡ���ID
               05  LF04-UID3                PIC X(36).
      * ������Ż���¸�γ�����
               05  LF04-CONFIRMDATE3        PIC X(10).
      * ���������� (mmlRd:code)
      *********05  LF04-SND-SPICOD1         PIC X(10).
               05  LF04-SND-SPICOD1         PIC X(50).
      * �����������η�̾ (mmlRd:system)
               05  LF04-SND-SPICODTBL1      PIC X(15).
      * ����̾
      *********05  LF04-SND-SPINAME1        PIC X(50).
               05  LF04-SND-SPINAME1        PIC X(80).
      * <��������>
      *********05  LF04-SNDSPITBL           OCCURS  3.
               05  LF04-SNDSPITBL           OCCURS  6.
      * ���������� (mmlRd:code)
                   07  LF04-SNDSPITBL-SPICOD2     PIC X(10).
      * �����������η�̾ (mmlRd:system)
                   07  LF04-SNDSPITBL-SPICODTBL2  PIC X(15).
      * ����̾
                   07  LF04-SNDSPITBL-SPINAME2    PIC X(20).
      * <ʬ��>
               05  LF04-SNDBNRTBL           OCCURS  4.
      * ʬ��μ��ॳ���� (mmlRd:tableId)
                   07  LF04-SNDBNRTBL-BNRTBL1         PIC X(07).
      * ʬ��̾
                   07  LF04-SNDBNRTBL-BNRNAME1        PIC X(30).
      * <����¾�����������>
      * ����������
               05  LF04-SND-SKNSTARTDATE    PIC X(10).
      * ������λ��
               05  LF04-SND-SKNENDDATE      PIC X(10).
      * ž��
               05  LF04-SND-TENKI           PIC X(20).
      * �����ν����
               05  LF04-SND-SKNSHOSHINDATE  PIC X(10).
      * �����ʴؤ����ݸ��η��ݸ��⥸�塼���UID (ñ�ȥ���) (mmlRd:uid)
               05  LF04-SND-SKNHLTUID       PIC X(36).
      * ������ʬ (mmlRd:InOutPatient)
               05  LF04-SND-NYUGAIKBN       PIC X(20).
      *
      *
      * [ͽ������(Claim)�⥸�塼��]
      * ʸ���ˡ���ID
           03  LF04-UID8                PIC X(36).
      * ������Ż���¸�γ�����
           03  LF04-CONFIRMDATE8        PIC X(10).
      * �ɥ�����������
           03  LF04-DOCCD               PIC X(10).
      * ͽ������⥸�塼��ξ��� (claim:status)
           03  LF04-STATUS              PIC X(07).
      * ��������ȯ������ (claim:orderTime)
           03  LF04-ORDER-TIME         PIC  X(19).
      *�»�ͽ������ (claim:appointTime)
           03  LF04-APPOINT-TIME        PIC X(19).
      * �������� (claim:registTime)
           03  LF04-REGIST-TIME         PIC X(19).
      *�»����� (claim:performTime)
           03  LF04-PERFORM-TIME        PIC X(19).
      * ���������ʬ (true OR false) (claim:admitFlag)
           03  LF04-NYUGAIKBN           PIC X(05).
      *���ֳ���ʬ (claim:timeClass)
           03  LF04-JIKANNGAIKBN        PIC X(01).
      * �����ݸ��գɣ� (claim:insuranceUid)
           03  LF04-HOKEN-UID           PIC X(36).
      * ���̰�����ᥳ�����η�
           03  LF04-DEFAULT-TABLED      PIC X(100).
      * <ͽ�����>
      * ͽ��
           03  LF04-YYK                PIC  X(20).
      * ͽ�󥳡��� (claim:appCode)
           03  LF04-YYKCOD             PIC  X(14).
      * ͽ�󥳡��ɤΥơ��֥�̾ (claim:appCodeId)
           03  LF04-YYKCODTBL          PIC  X(8).
      * ͽ����
           03  LF04-YYKMEMO            PIC  X(100).
      * <���ݸ�����>
      * ���ݸ�����
           03  LF04-HKNSBT              PIC X(20).
      * ���ݸ����̥����� (mmlHi:ClassCode)
           03  LF04-HKNSBTCODE          PIC X(2).
      * ���ݸ����̥����ɥơ��֥�̾ (mmlHi:tableId) (MML0031)
           03  LF04-HKNSBTTBL           PIC X(7).
      * <���Ųʼ»ܾ���>
      * ���Ų�̾��
           03  LF04-SRYKA-NAME          PIC X(12).
      * ɽ��ˡ (mmlDp:repCode)
           03  LF04-SRYKA-REPCODE       PIC X(01).
      * ɽ��ˡ�Υơ��֥�̾ (mmlDp:tableId) (MML0025)
           03  LF04-SRYKA-REPCODETBL    PIC X(07).
      * ���Ų�ID
           03  LF04-SRYKA-ID            PIC X(02).
      * ID�μ��ॳ���� (mmlCm:type)
           03  LF04-SRYKA-IDCODE        PIC X(07).
      * ID�μ��ॳ���ɤΥơ��֥�̾ (mmlCm:tableId) (MML0029)
           03  LF04-SRYKA-IDCODETBL     PIC X(07).
      * �����å��ǥ����å��� (mmlCm:checkDigit)
           03  LF04-SRYKA-CD            PIC X(02).
      * �����å��ǥ����å����� (mmlCm:checkDigitSchema)
           03  LF04-SRYKA-CDSCHEMA      PIC X(03).
      * <���ż»��������>
      * ���ż»�����̾��
           03  LF04-SNRBT-NAME          PIC X(12).
      * ɽ��ˡ (mmlDp:repCode)
           03  LF04-SNRBT-REPCODE       PIC X(01).
      * ɽ��ˡ�Υơ��֥�̾ (mmlDp:tableId) (MML0025)
           03  LF04-SNRBT-REPCODETBL    PIC X(07).
      * ��������ID
           03  LF04-SNRBT-ID            PIC X(02).
      * ID�μ��ॳ���� (mmlCm:type)
           03  LF04-SNRBT-IDCODE        PIC X(07).
      * ID�μ��ॳ���ɤΥơ��֥�̾ (mmlCm:tableId) (MML0029)
           03  LF04-SNRBT-IDCODETBL     PIC X(07).
      * �����å��ǥ����å��� (mmlCm:checkDigit)
           03  LF04-SNRBT-CD            PIC X(02).
      * �����å��ǥ����å����� (mmlCm:checkDigitSchema)
           03  LF04-SNRBT-CDSCHEMA      PIC X(03).
      * <���ų�� 1>
           03  LF04-SRYTBL-G.
             04  LF04-SRYTBL             OCCURS  40.
      * ���Ź԰ٶ�ʬ������ (claim:classCode)
               05  LF04-SRYCD          PIC  X(03).
      * ���Ź԰ٶ�ʬ�����ɤΥơ��֥�̾ (claim:classCodeId) (Claim002)
               05  LF04-SRYCDTBL       PIC  X(08).
      * ���Ź԰ٶ�ʬ̾
               05  LF04-SRYNAME        PIC  X(40).
      * ��ˡ
               05  LF04-ADMINISTRATION
                                       PIC  X(50).
      * ��ˡ������ (claim:adminCode)
               05  LF04-ADMINCODE      PIC  X(09).
      * ��ˡ�����ɤΥơ��֥�̾ (claim:adminCodeId) (Claim006)
               05  LF04-ADMINCODETBL   PIC  X(08).
      * ��ˡ������
               05  LF04-ADMMEMO        PIC  X(100).
      * ����⤷��������(Z�Խ�)
               05  LF04-KAISU          PIC  X(50).
      * <���٥ơ��֥�(���Ź��� 1)>
               05  LF04-SRYTBL1       OCCURS  40.
      * ���ż��̶�ʬ������(0:�굻��1:����,2:����) (claim:subclassCode)
                   07  LF04-SRYSYUBETSU
                                       PIC  X(01).
      * ���ż��̶�ʬ�����ɤΥơ��֥�̾ (claim:subclassCodeId) (Claim003)
                   07  LF04-SRYSYUBETSUTBL
                                       PIC  X(08).
      * ������ᥳ����(S + ���Ź԰٥�����) (claim:code)
                   07  LF04-IJICD      PIC  X(10).
      * ������ᥳ���ɤΥơ��֥�̾ (claim:tableId)
                   07  LF04-TBLID      PIC  X(20).
      * �裲������ᥳ���� (claim:aliasCode)
                   07  LF04-ALSCODE    PIC  X(10).
      * �裲������ᥳ���ɤΥơ��֥�̾ (claim:aliasTableId)
                   07  LF04-ALSTBLID   PIC  X(20).
      * �������̾��
      *R02.4       07  LF04-ALSNAME    PIC  X(80).
                   07  LF04-ALSNAME    PIC  X(140).
      * ����(Z�Խ�)
      *****        07  LF04-SURYO      PIC  X(12).
                   07  LF04-SURYO      PIC  X(40).
      * ���̥����� (claim:numberCode)
                   07  LF04-SURYOCD    PIC  X(02).
      * ���̥����ɤΥơ��֥�̾ (claim:numberCodeId) (Claim004)
                   07  LF04-SURYOCDTBL PIC  X(08).
      * ñ�� (claim:unit)
                   07  LF04-TANI       PIC  X(20).
      * ���׻���(PTnHnM,5����23ʬ��PT5H23M)
                   07  LF04-DURATION   PIC  X(19).
      * ����
                   07  LF04-LOCATION   PIC  X(50).
      * <�ե����>
                   07  LF04-FILM       OCCURS  6.
      * �ե���ॵ����
                     09  LF04-FILMSIZE PIC  X(50).
      * �ե���ॵ���������� (claim:sizeCode)
                     09  LF04-FILMSIZECD
                                       PIC  X(02).
      * �ե���ॵ���������ɤΥơ��֥�̾ (claim:sizeCodeId)
                     09  LF04-FILMSIZECDTBL
                                       PIC  X(08).
      * �ե����ʬ�� (claim:filmDivision)
                     09  LF04-FILMDEV  PIC  X(02).
      * �ե�������(Z�Խ�)
                     09  LF04-FILMNUM  PIC  X(06).
      * <���Ź��� 2>
      * ��Ϣ�굻
                   07  LF04-EVENT      PIC  X(50).
      * ��Ϣ�굻 ������ (claim:eventStart)
                   07  LF04-EVENTSTART PIC  X(10).
      * ��Ϣ�굻 ��λ�� (claim:eventEnd)
                   07  LF04-EVENTEND   PIC  X(10).
      * ���Ź��ܥ��
                   07  LF04-SRYMEMO    PIC  X(100).
      * <���ų�� 2>
      * ���ų����
               05  LF04-MEMO           PIC  X(100).
      *AIP��
      *UID
           03  LF04-MEDICAL-UID       PIC X(36).
      *Ʊ����������ʬ
           03  LF04-DOUJITSUKBN       PIC X(01).
      *ver.4.7
      *��̾��­������
           03  LF04-BYOMEI-TBL        OCCURS   50.
      *        ��­�����ȥ�����
               05  LF04-HOSOKUCD-G.
                   07  LF04-HOSOKUCD      PIC X(10)
                                          OCCURS   3.
      *        ��­������
               05  LF04-HOSOKU-COMMENT    PIC X(40).
      *ver.4.8
      *API ���Ź԰�
           03  LF04-SRYACCT-TBL1       OCCURS   40.
               05  LF04-SRYACCT-TBL2       OCCURS   40.
      *            ��³�����Ȼؼ���ʬ
                   07  LF04-CONTKBN        PIC X(01).
      *            �����������ʬ
                   07  LF04-NAIFKBN        PIC X(01).
      *