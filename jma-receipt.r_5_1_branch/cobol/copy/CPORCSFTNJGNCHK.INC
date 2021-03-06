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
      *    ���Լ�����ô��쥻�ץȵ���Ƚ��ѥ�᥿                 *
      *                         COPY  (CPORCSFTNJGNCHK.INC)       *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  03.05.01    NACL-ƣ��    07/04/19  ���롼�׿����б�
      *  04.03.01    NACL-����    08/09/01  ��ƻ�ܸ��ֹ��ɲ�
      *  04.03.02    NACL-����    08/12/17  �ݸ��ȹ礻�ֹ桢���ݸ��ݣɣ��ɲ�
      *  04.03.03    NACL-����    09/01/13  ������ô�ɲ�
      *  04.07.01    NACL-����    11/09/01  �����ƥ��������������ǽ�ɲ��б�
      *                                     ��������ơ��֥�����ե饰�ɲ�
      *  04.07.02    NACL-����    11/12/13  ���������Τߤ������ѥ쥻�ץ��б�
      *                                     ������������������ʬ��
      *                                     �����ѥ쥻�ץȣ������񵭺���
      *                                     �����������ɲ�
      *  04.08.01    NACL-����    15/10/13  ���᥼���ե饰�ɲ�
      ************************************************************
       01  ORCSFTNJGNCHKAREA.
      *������
      *    ���ŵ��أɣ�
           03  LNK-FTNJGNCHK-HOSPNUM         PIC 9(2).
      *    ���ԣɣ�
           03  LNK-FTNJGNCHK-PTID            PIC 9(10).
      *    ������ʬ
           03  LNK-FTNJGNCHK-NYUGAIKBN       PIC X(01).
      *    ����ǯ��
           03  LNK-FTNJGNCHK-SRYYM           PIC X(06).
      *    �����ֹ�
           03  LNK-FTNJGNCHK-KOHNUM          PIC X(03).
      *    ����ɣ�
           03  LNK-FTNJGNCHK-KOHID           PIC 9(10).
      *    ��ƻ�ܸ��ֹ�
           03  LNK-FTNJGNCHK-PREFNUM         PIC 9(02).
      *    �ݸ��ȹ礻�ֹ�
           03  LNK-FTNJGNCHK-HKNCOMBINUM     PIC 9(04).
      *    ���ݸ��ݣɣ�
           03  LNK-FTNJGNCHK-HKNID           PIC 9(10).
      *����������
      *    00:�쥻�ץȵ��ܤ���
      *    01:�쥻�ץȵ��ܤʤ�
      *    10:�ѥ�᥿���顼
           03  LNK-FTNJGNCHK-RC              PIC 9(02).
      *    �����ѥ쥻�ץȣ������񵭺���
           03  LNK-FTNJGNCHK-TENKEN-RC       PIC 9(02).
      *
           03  LNK-FTNJGNCHK-KOHFTN          PIC 9(07).
           03  LNK-FTNJGNCHK-KOHFTNSKJ       PIC 9(07).
           03  LNK-FTNJGNCHK-KOHSKYFLG       PIC X(01).
           03  LNK-FTNJGNCHK-SKYZEROFLG      PIC X(01).
