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
      *    �쥻�����Խ��ѥ�᥿����COPY (CPORCSRECESORT.INC)
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  03.05.01    NACL-ƣ��    07/07/10  ��������ɲ�
      *
      *  04.05.01    NACL-ƣ��    09/11/27  �쥻�ť����ȳ�ĥ�б�
      *
      *  04.06.01    NACL-ƣ��    10/10/12  ����ñ�ȤΥ쥻ʬΥ�б�
      *                                     kohid �ɲ�
      *  04.06.02    NACL-ƣ��    12/10/26  �õ����ࣰ����Ϸʻ�˵ڤ�
      *                                     ������Ϸ��ˤΥ쥻�ץ��б�
      ************************************************************
       01  ORCSRECESORTAREA.
      *
      *����
           03  ORCSRECESORT-IN-AREA.
      *���ŵ��أɣ�
               05  ORCSRECESORT-HOSPNUM         PIC  9(02).
      *������ʬ
               05  ORCSRECESORT-NYUGAIKBN       PIC  X(01).
      *���ԣɣ�
               05  ORCSRECESORT-PTID            PIC  9(10).
      *����ǯ��
               05  ORCSRECESORT-SRYYM           PIC  X(06).
      *���Ŷ�ʬ
               05  ORCSRECESORT-SRYKBN          PIC  X(02).
      *���ֹ�
               05  ORCSRECESORT-ZAINUM          PIC  9(08).
      *�������
               05  ORCSRECESORT-KIZAITEN        PIC  9(07).
      *������
               05  ORCSRECESORT-IN-TBL.
                   07  ORCSRECESORT-IN-SRYCD-G  OCCURS   50.
      *                ��Ϣ�����ȸ����ѣˣţ�
                       09  ORCSRECESORT-IN-KEY.
                           11  ORCSRECESORT-IN-KEY-HOSPNUM   PIC 9(2).
                           11  ORCSRECESORT-IN-KEY-SRYYM     PIC 9(06).
                           11  ORCSRECESORT-IN-KEY-NYUGAIKBN PIC X(01).
                           11  ORCSRECESORT-IN-KEY-PTID      PIC 9(10).
                           11  ORCSRECESORT-IN-KEY-RECEKA    PIC X(02).
                           11  ORCSRECESORT-IN-KEY-TEISYUTUSAKI
                                                             PIC 9(01).
                           11  ORCSRECESORT-IN-KEY-RECESYUBETU
                                                             PIC 9(04).
                           11  ORCSRECESORT-IN-KEY-HKNJANUM  PIC X(08).
                           11  ORCSRECESORT-IN-KEY-HOJOKBN   PIC X(01).
                           11  ORCSRECESORT-IN-KEY-KOHID     PIC 9(10).
                           11  ORCSRECESORT-IN-KEY-TEKSTYMD  PIC X(08). 
                           11  ORCSRECESORT-IN-KEY-RECEIPT-KBN
                                                             PIC X(10).
                           11  ORCSRECESORT-IN-KEY-ZAINUM    PIC 9(08).
                           11  ORCSRECESORT-IN-KEY-ZAIREN    PIC 9(02).
                           11  ORCSRECESORT-IN-KEY-RENNUM    PIC 9(02).
                       09  ORCSRECESORT-IN-SHORI-RENNUM
                                                    PIC 9(01).
      *                ���ť�����
                       09  ORCSRECESORT-IN-SRYCD    PIC X(09).
      *                �ǡ�����ʬ
                       09  ORCSRECESORT-IN-DATAKBN  PIC X(01).
      *                ����
                       09  ORCSRECESORT-IN-SRYSURYO PIC  9(05)V9(05).
      *                ���
                       09  ORCSRECESORT-IN-SRYKAISU PIC  9(03).
      *                ñ�̥�����
                       09  ORCSRECESORT-IN-TANICD   PIC  9(03).
      *                �������̶�ʬ
                       09  ORCSRECESORT-IN-TENSIKIBETU
                                                    PIC  9(01).
      *                ����
                       09  ORCSRECESORT-IN-TEN      PIC  9(09)V9(02).
      *                ����̾��
                       09  ORCSRECESORT-IN-NAME     PIC  X(200).
      *                ��������̣�
                       09  ORCSRECESORT-IN-TOKUKIZAISBT1 
                                                    PIC X(01).
      *                ��������ʬ
                       09  ORCSRECESORT-IN-SANSOKBN PIC X(01).
      *                �쥻���������ྦ��̾
                       09  ORCSRECESORT-IN-RECEDENNAME
                                                    PIC X(300).
      *����
           03  ORCSRECESORT-OUT-AREA.
      *        ���ơ��֥�ʣ����ե���ࡢ�������ǡ��������ǡ���������¾����
      *                     �����ե���ࣲ�������ե���ࣵ��
               05  ORCSRECESORT-DATE-TBL       OCCURS   8.
      *            ����
                   07  ORCSRECESORT-ZAITEN     PIC  9(08).
      *            �ͣ���
                   07  ORCSRECESORT-TBLMAX     PIC  9(02).
      *
                   07  ORCSRECESORT-SRYCD-G         OCCURS   50.
      *                ��Ϣ�����ȸ����ѣˣţ�
                       09  ORCSRECESORT-KEY.
                           11  ORCSRECESORT-KEY-HOSPNUM    PIC 9(2).
                           11  ORCSRECESORT-KEY-SRYYM      PIC 9(06).
                           11  ORCSRECESORT-KEY-NYUGAIKBN  PIC X(01).
                           11  ORCSRECESORT-KEY-PTID       PIC 9(10).
                           11  ORCSRECESORT-KEY-RECEKA     PIC X(02).
                           11  ORCSRECESORT-KEY-TEISYUTUSAKI
                                                       PIC 9(01).
                           11  ORCSRECESORT-KEY-RECESYUBETU
                                                       PIC 9(04).
                           11  ORCSRECESORT-KEY-HKNJANUM   PIC X(08).
                           11  ORCSRECESORT-KEY-HOJOKBN    PIC X(01).
                           11  ORCSRECESORT-KEY-KOHID      PIC 9(10).
                           11  ORCSRECESORT-KEY-TEKSTYMD   PIC X(08). 
                           11  ORCSRECESORT-KEY-RECEIPT-KBN
                                                           PIC X(10).
                           11  ORCSRECESORT-KEY-ZAINUM     PIC 9(08).
                           11  ORCSRECESORT-KEY-ZAIREN     PIC 9(02).
                           11  ORCSRECESORT-KEY-RENNUM     PIC 9(02).
                       09  ORCSRECESORT-SHORI-RENNUM
                                                    PIC 9(01).
      *                ���ť�����
                       09  ORCSRECESORT-SRYCD       PIC X(09).
      *                �ǡ�����ʬ
                       09  ORCSRECESORT-DATAKBN     PIC X(01).
      *                ����
                       09  ORCSRECESORT-SRYSURYO    PIC  9(05)V9(05).
      *                ���
                       09  ORCSRECESORT-SRYKAISU    PIC  9(03).
      *                ñ�̥�����
                       09  ORCSRECESORT-TANICD      PIC  9(03).
      *                �������̶�ʬ
                       09  ORCSRECESORT-TENSIKIBETU PIC  9(01).
      *                ����
                       09  ORCSRECESORT-TEN         PIC  9(09)V9(02).
      *                ����̾��
                       09  ORCSRECESORT-NAME        PIC  X(200).
      *                �쥻���������ྦ��̾
                       09  ORCSRECESORT-RECEDENNAME PIC  X(300).
      *���顼�����ɡʣ����ޤʤ���
           03  ORCSRECESORT-ERRCD           PIC 9(01).
      *
