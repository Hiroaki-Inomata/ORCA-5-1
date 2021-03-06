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
      *    ����ץǡ����ʴ��ܾ����     COPY (CPTCF002.INC)    *
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  03.05.01    NACL-ƣ��    07/04/19  ���롼�׿����б�
      *
      *  04.05.01    NACL-ƣ��    12/11/15  ���������Υޥ��ʥ�ɽ���б�
      *  04.05.02    NACL-ƣ��    10/10/12  ����ñ�ȤΥ쥻ʬΥ�б�
      *                                     ������kohid �ɲ�
      *
      *  04.06.01    NACL-ƣ��    12/10/26  �õ����ࣰ����Ϸʻ�˵ڤ�
      *                                     ������Ϸ��ˤΥ쥻�ץ��б�
      ************************************************************
      *01  TOUKEI02-REC.
           03  TOUKEI02-KEY.
               05  TOUKEI02-KEY1.
      *            ���ŵ��أɣ�
                   07  TOUKEI02-HOSPNUM    PIC 9(2).
      *            ����ǯ��
                   07  TOUKEI02-SRYYM      PIC  9(06).
      *            ������ʬ
                   07  TOUKEI02-NYUGAIKBN  PIC  X(01).
      *            ���ԣɣ�
                   07  TOUKEI02-PTID       PIC  9(10).
      *            �������ʬ
                   07  TOUKEI02-RECEIPT-KBN
                                           PIC  X(10).
      *            �쥻�ץȿ��Ų�
                   07  TOUKEI02-RECEKA     PIC  X(02).
      *            ���ݸ��ݸ��ֹ�
                   07  TOUKEI02-HKNNUM     PIC  X(03).
      *            ����ɣ�
                   07  TOUKEI02-KOHID      PIC  9(10).
      *            �ݸ����ֹ�        
                   07  TOUKEI02-HKNJANUM   PIC  X(08).
      *            �ܿͲ�²��ʬ
                   07  TOUKEI02-HONKZKKBN  PIC  X(01).
      *            �����ʬ
                   07  TOUKEI02-HOJOKBN    PIC  X(01).
      *            ����ɣġʸ���ñ�Ȼ����ѡ�
                   07  TOUKEI02-KOH-KOHID  PIC  9(10).
      *        ���Ų�
               05  TOUKEI02-SRYKA          PIC  X(02).
      *        ���Ŷ�ʬ
               05  TOUKEI02-SRYKBN         PIC  X(02).
      *        ���ֹ�
               05  TOUKEI02-ZAINUM         PIC  9(08).
      *        Ϣ��
               05  TOUKEI02-SEQNO          PIC  9(03).
      *Ϣ�ֹ�
           03  TOUKEI02-RENNUM             PIC  9(02).
      *�ݸ��ȹ礻
           03  TOUKEI02-HKNCOMBI           PIC  9(04).
      *
      *���ݸ����ݸ��ɣ�
           03  TOUKEI02-HKNID              PIC  9(10).
      *����ɣ�
           03  TOUKEI02-KOHID-AREA.
               05  TOUKEI02-TKOHID         PIC  9(10)   OCCURS  4.
      *�쥻�ſ��Ų�
           03  TOUKEI02-RECESRYKA          PIC  X(02).
      *���ݶ�ʬ
           03  TOUKEI02-JIHOKBN            PIC  X(01).
      *������
           03  TOUKEI02-ZAITEN             PIC  S9(08).
      *�޲��
           03  TOUKEI02-ZAIKAISU           PIC  9(04).
      *���ٿ�
           03  TOUKEI02-MEISAISU           PIC  9(04).
      *�굻������
           03  TOUKEI02-SYUTEN1            PIC  S9(07).
      *�굻������
           03  TOUKEI02-SYUTEN2            PIC  S9(07).
      *��������
           03  TOUKEI02-YKZTEN             PIC  S9(07).
      *�������
           03  TOUKEI02-KIZAITEN           PIC  S9(07).
      *�ɥ�����������
           03  TOUKEI02-DRCD               PIC  X(05).
      *��������ơ��֥�
           03  TOUKEI02-DAY-AREA.
      *        ���Ų���ơ��֥��1:���,2:����,3:����, 4:����ʾ��
               05  TOUKEI02-KAISUINF       OCCURS  31.
                   07  TOUKEI02-KAISU      PIC  9(03).
      *���Ź԰پ���
           03  TOUKEI02-SRYACTINF.
      *���ż��̶�ʬ
               05  TOUKEI02-SRYSYUKBN      PIC  X(03).
      *���ť�����
               05  TOUKEI02-SRYCD          PIC  X(09).
      *����
               05  TOUKEI02-SRYSURYO       PIC  9(05)V9(05).
      *���
               05  TOUKEI02-SRYKAISU       PIC  9(03).
      *�������̶�ʬ
               05  TOUKEI02-TENSIKIBETU    PIC  9(01).
      *����
               05  TOUKEI02-TEN            PIC  9(09)V9(02).
      *̾�������ֹ�
               05  TOUKEI02-INPUTNUM       PIC  9(03).
      *������
               05  TOUKEI02-JIHIMONEY      PIC  9(07).
      *ñ�̥�����
               05  TOUKEI02-TANICD         PIC  X(03).
      *ñ��̾��
               05  TOUKEI02-TANINAME       PIC  X(24).
      *����̾��
               05  TOUKEI02-NAME           PIC  X(200).
      *����̾��
               05  TOUKEI02-FORMALNAME     PIC  X(400).
      *������ॳ����
               05  TOUKEI02-YAKKAKJNCD     PIC  X(12).
      *�����󽸷��輱��
               05  TOUKEI02-TENTTLKBN      PIC  9(03).
