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
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      ******************************************************************
      *    ���Уɥꥯ�����ȣạ̃��ɣ����ӥǡ��������� (xml2)
      *                                  COPY (CPCLAIMRECEIVEV02REQ.INC)
      ******************************************************************
       01  XML-CLAIM-RECEIVEV2REQ.
           02  CLREREQ-CONTEXT           PIC S9(9)   BINARY.
           02  CLREREQ-OBJECT            PIC X(36).
           02  CLREREQ-MODE              PIC S9(9)   BINARY.
           02  CLREREQ-RECORDNAME        PIC X(128).
           02  CLREREQ-RECEIVE-FRONT.
      * ������ ����ID
               03  CLREREQ-CRE001-FACILITYID   PIC X(20).
      * ���Լ�ID
               03  CLREREQ-PTID1               PIC X(20).
      * ��������ȯ������ (claim:orderTime)
               03  CLREREQ-ORDER-TIME          PIC  X(19).
      *�»����� (claim:performTime)
               03  CLREREQ-PERFORM-TIME        PIC X(19).
      * ������ ���Ų�ID
               03  CLREREQ-CRE001-DPERTID      PIC X(20).
      *
      * [���ݸ��⥸�塼��]
               03  CLREREQ-HKNTBL.
      * ���ݸ�����
                   05  CLREREQ-HKN-HKNSBT          PIC X(20).
      * �ݸ����̥����� (mmlHi:ClassCode)
                   05  CLREREQ-HKN-HKNSBTCODE      PIC X(2).
      * ���ݸ��ֹ�
                   05  CLREREQ-HKN-HKNJANUM        PIC  X(08).
      * ���ݸ��Ե���
                   05  CLREREQ-HKN-KIGO            PIC  X(80).
      * ���ݸ����ֹ�
                   05  CLREREQ-HKN-NUM             PIC  X(80).
      * <������ô���ž���>
                   05  CLREREQ-KOHINF       OCCURS  4.
      * ������ô̾��
                       07  CLREREQ-HKNKOH-PROVIDER     PIC  X(20).
      * ��ô���ֹ�
                       07  CLREREQ-HKNKOH-FTNJANUM     PIC  X(08).
      * ������ֹ�
                       07  CLREREQ-HKNKOH-JKYSNUM      PIC  X(20).
      *
      * [��������⥸�塼��]
               03  CLREREQ-SNDRRKTBL           OCCURS  50.
      * ���������� (mmlRd:code)
                   05  CLREREQ-SND-SPICOD1         PIC X(50).
      * ����̾
                   05  CLREREQ-SND-SPINAME1        PIC X(80).
      * <��������>
                   05  CLREREQ-SNDSPITBL           OCCURS  6.
      * ���������� (mmlRd:code)
                       07  CLREREQ-SNDSPITBL-SPICOD2     PIC X(10).
      * ����̾
                       07  CLREREQ-SNDSPITBL-SPINAME2    PIC X(20).
      * <ʬ��>
                   05  CLREREQ-SNDBNRTBL           OCCURS  4.
      * ʬ��μ��ॳ���� (mmlRd:tableId)
                       07  CLREREQ-SNDBNRTBL-BNRTBL1         PIC X(07).
      * ʬ��̾
                       07  CLREREQ-SNDBNRTBL-BNRNAME1        PIC X(30).
      * <����¾�����������>
      * ����������
                   05  CLREREQ-SND-SKNSTARTDATE    PIC X(10).
      * ������λ��
                   05  CLREREQ-SND-SKNENDDATE      PIC X(10).
      * ž��
                   05  CLREREQ-SND-TENKI           PIC X(20).
      * ������ʬ (mmlRd:InOutPatient)
                   05  CLREREQ-SND-NYUGAIKBN       PIC X(20).
      *
      *
      * [ͽ������(Claim)�⥸�塼��]
      * ʸ���ˡ���ID
               03  CLREREQ-UID8                PIC X(36).
      * �ɥ�����������
               03  CLREREQ-DOCCD               PIC X(10).
      * ���������ʬ (true OR false) (claim:admitFlag)
               03  CLREREQ-NYUGAIKBN           PIC X(05).
      *���ֳ���ʬ (claim:timeClass)
               03  CLREREQ-JIKANNGAIKBN        PIC X(01).
      * �����ݸ��գɣ� (claim:insuranceUid)
               03  CLREREQ-HOKEN-UID           PIC X(36).
      * <���ų�� 1>
               03  CLREREQ-SRYTBL-G.
                 04  CLREREQ-SRYTBL             OCCURS  40.
      * ���Ź԰ٶ�ʬ������ (claim:classCode)
                   05  CLREREQ-SRYCD          PIC  X(03).
      * ���Ź԰ٶ�ʬ�����ɤΥơ��֥�̾ (claim:classCodeId) (Claim002)
                   05  CLREREQ-SRYCDTBL       PIC  X(08).
      * ��ˡ������ (claim:adminCode)
                   05  CLREREQ-ADMINCODE      PIC  X(09).
      * ��ˡ������
                   05  CLREREQ-ADMMEMO        PIC  X(100).
      * ����⤷��������(Z�Խ�)
                   05  CLREREQ-KAISU          PIC  X(50).
      * <���٥ơ��֥�(���Ź��� 1)>
                   05  CLREREQ-SRYTBL1       OCCURS  40.
      * ������ᥳ����(S + ���Ź԰٥�����) (claim:code)
                       07  CLREREQ-IJICD      PIC  X(10).
      * ������ᥳ���ɤΥơ��֥�̾ (claim:tableId)
                       07  CLREREQ-TBLID      PIC  X(20).
      * �������̾��
                       07  CLREREQ-ALSNAME    PIC  X(80).
      * ����(Z�Խ�)
                       07  CLREREQ-SURYO      PIC  X(40).
      * ���̥����� (claim:numberCode)
                       07  CLREREQ-SURYOCD    PIC  X(02).
      * ���Ź��ܥ��
                       07  CLREREQ-SRYMEMO    PIC  X(100).
      * ���ų����
                   05  CLREREQ-MEMO           PIC  X(100).
      *
