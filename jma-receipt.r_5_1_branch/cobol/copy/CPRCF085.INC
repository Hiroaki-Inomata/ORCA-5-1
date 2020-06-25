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
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      ************************************************************
      *    ϫ�ҥ쥻�ץ����ٽ��������     COPY (CPRCF085.INC)    *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-����    04.06.07  �����֤����Ĺ��ܤ��ѹ�
      *
      *  02.06.01    NACL-ƣ��    05/08/22  ���θ��������Ȥ��ɲ�
      *
      *  02.09.01    NACL-ƣ��    06/04/13  �������ͷ׻�����
      *                                     �������»�Ƚ�ǥ��롼�פ��ɲ�
      *  03.05.01    NACL-����    07.06.01  ���롼�׿����б�
      *  04.01.01    NACL-����    07/11/26  �����ྦ��̾�����б�
      *
      *  04.05.01    NACL-ƣ��    09/07/14  �������ʲ������б�
      *
      *  04.07.01    NACL-����    13/01/21  ϫ�ҥ쥻���б�
      *
      *  04.08.01    NACL-����    16/01/12  �����ѥ쥻�ץȱ�������
      *                                     ����̾�����б�
      *
      *  05.00.01    NACL-����    20/03/30  ���£�ǯ��������б�
      ************************************************************
      *01  RECE85-REC.
           03  RECE85-KEY.
               05  RECE85-KEY1.
      *���ŵ��أɣ�
                   07  RECE85-HOSPNUM     PIC  9(02).
      *����ǯ��
                   07  RECE85-SRYYM       PIC  9(06).
      *������ʬ
                   07  RECE85-NYUGAIKBN   PIC  X(01).
      *���ԣɣ�
                   07  RECE85-PTID        PIC  9(10).
      *�쥻�ץȲ�
                   07  RECE85-RECEKA      PIC  X(02).
      *�ݸ��ɣ�        
                   07  RECE85-HKNID       PIC  9(10).
      *Ŭ�ѳ���ǯ����
                   07  RECE85-TEKSTYMD    PIC  X(08).
      *���ֹ�
               05  RECE85-ZAINUM          PIC  9(08).
      *Ϣ�ֹ�
               05  RECE85-RENNUM          PIC  9(02).
      *�����ֹ�
           03  RECE85-PTNUM               PIC  X(20).
      *���Ų�
           03  RECE85-SRYKA               PIC  X(02).
      *���Ŷ�ʬ
           03  RECE85-SRYKBN              PIC  X(02).
      *���ż��̶�ʬ
           03  RECE85-SRYSYUKBN           PIC  X(03).
      *������
           03  RECE85-JIHIMONEY           PIC  9(07).
      *
           03  RECE85-SRYACT-TBL          OCCURS  5.
      *���Ź԰پ���
             05  RECE85-SRYACTINF.
      *���ť�����
                 07  RECE85-SRYCD         PIC  X(09).
      *���ż��̶�ʬ
                 07  RECE85-SRYSYUKBN-X   PIC  X(03).
      *����
                 07  RECE85-SRYSURYO      PIC  9(05)V9(05).
      *���
                 07  RECE85-SRYKAISU      PIC  9(03).
      *ñ�̥�����
                 07  RECE85-TANICD        PIC  9(03).
      *ñ��̾��ͭ�����
                 07  RECE85-TANIMOJISU    PIC  9(01).
      *ñ��̾��
                 07  RECE85-TANINAME      PIC  X(24).
      *�������̶�ʬ
                 07  RECE85-TENSIKIBETU   PIC  9(01).
      *����
                 07  RECE85-TEN           PIC  9(09)V9(02).
      *��������ե饰
                 07  RECE85-MEISKYFLG     PIC  X(01).
      *��ư�����ʬ
                 07  RECE85-AUTOKBN       PIC  X(01).
      *̾�������ֹ�
                 07  RECE85-INPUTNUM      PIC  9(03).
      *�����󽸷��輱��
                 07  RECE85-TENTTLKBN     PIC  9(03).
      *�ǡ�����ʬ
                 07  RECE85-DATAKBN       PIC  9(01).
      *����оݸ���
                 07  RECE85-HOUKNSKBN     PIC  9(02).
      *�������»�Ƚ��
                 07  RECE85-KNSJISKBN     PIC  9(01).
      *�������»�Ƚ�ǥ��롼��
                 07  RECE85-KNSJISGRPKBN  PIC  9(02).
      *�Ƽ��ֹ���
                 07  RECE85-CDKBNINF.
                     09  RECE85-CDKBN-KBN        PIC  X(01).
                     09  RECE85-CDKBN-SYO        PIC  9(01).
                     09  RECE85-CDKBN-BU         PIC  9(02).
                     09  RECE85-CDKBN-KBNNUM     PIC  9(03).
                     09  RECE85-CDKBN-KBNNUM-EDA PIC  9(02).
                     09  RECE85-CDKBN-KOUBAN     PIC  9(02).
                     09  RECE85-KOUHYOJYUNNUM    PIC  9(09).
      *�������̶�ʬ��
                 07  RECE85-KOKUJISKBKBN1 PIC  9(01).
      *�������̶�ʬ��
                 07  RECE85-KOKUJISKBKBN2 PIC  9(01).
      *��§�û��оݳ���ʬ
                 07  RECE85-TUSOKUGAIKBN  PIC  9(01).
      *���ֳ��û���ʬ
                 07  RECE85-TIMEKSNKBN    PIC  9(01).
      *�������ͷ׻�����
                 07  RECE85-KZMCOMPSIKIBETU
                                          PIC  9(01).
      *���û�������
                 07  RECE85-CHUKSNINF.
      *������
                     09  RECE85-CHUKSNCD         PIC  X(04).
      *����
                     09  RECE85-CHUKSNTUBAN      PIC  X(01).
      *���θ���������
                 07  RECE85-KENTAICOMMENT        PIC  9(01).
      *���ϥ�����
                 07  RECE85-INPUTCOMENT          PIC  X(140).
      *̾�ξ���
             05  RECE85-NAMEINF.
      *����ͭ�����
                 07  RECE85-YUKOKETA      PIC  9(03).
      *����̾��
                 07  RECE85-NAME          PIC  X(300).
      *���̥ե饰����
             05  RECE85-SIKFLGINF.
      *���̥ե饰
                 07  RECE85-SIKFLG        PIC  X(01).
      *��������̣�
             05  RECE85-TOKUKIZAISBT1     PIC  9(01).
      *��������ʬ
             05  RECE85-SANSOKBN          PIC  9(01).
      *�쥻�ץ��Խ�����
             05  RECE85-SSTKIJUNCDTBL     OCCURS 10.
                 07  RECE85-SSTKIJUNCD    PIC  9(03).
      *����¾����
             05  RECE85-SONOTAINF.
      *�쥻�ץ��Ż��оݳ��ե饰
                 07  RECE85-RECEDENFLG       PIC  X(01).
      *�쥻�ץ��оݳ��ե饰
                 07  RECE85-RECEPTFLG        PIC  X(01).
      *�쥻���������ྦ��̾
                 07  RECE85-RECEDENNAME      PIC  X(300).
      *�쥻�������ྦ��̾�Խ��ե饰
                 07  RECE85-RECEHENFLG       PIC  X(01).
      *�ط������ȴ�Ϣ����ե饰
                 07  RECE85-SETCOMATFLG      PIC  9(03).
      *
      *���ž���Ϣ���Ѿ���
             05  RECE85-IRYORENKEIINF.
      *����̾���ܱ��������ե饰
                 07  RECE85-INGAIFLG         PIC  X(01).
      *����̾���ܱ��������ե饰�ʥ쥻�ץ��ѡ�
                 07  RECE85-INGAIFLG-RECE    PIC  X(01).
      *����̾�����ɡ�������ॳ���ɡ�
                 07  RECE85-YAKKAKJNCD       PIC  X(12).
      *����̾�����û���ʬ
                 07  RECE85-GENERIC-KBN      PIC  X(01).
      *����̾��
                 07  RECE85-GENERICNAME      PIC  X(200).
      *����ͭ�����
                 07  RECE85-GENERICYUKOKETA  PIC  9(03).
      *