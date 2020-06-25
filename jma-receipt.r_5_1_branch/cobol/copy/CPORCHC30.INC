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
      *----------------------------------------------------------*
      *    ���޾�����ϥѥ�᥿              COPY (CPORCHC30.INC)
      *                                          (ORCHC30 �ǻ��ѡ�
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     NACL-¿��Ǽ  06/01/24  ���̳�祳�����ɲ�
      * 04.01.00     NACL-¿��Ǽ  07/12/11  �����ɲ�
      * 04.05.00     NACL-����    09/06/22  �������ʲ�������ѹ�
      * 04.05.00     NACL-¿��Ǽ  09/07/23  �������ޡ��������ɲ�
      * 04.06.00     NACL-¿��Ǽ  11/02/22  ��ˡ�������б��ɲ�
      * 04.08.00     NACL-¿��Ǽ  16/10/XX  ��ˡ�����ɿ����ɲ�
      ************************************************************
      *
       01  ORCHC30AREA.
      *
      *    ������
           03  ORCHC30-MAX             PIC 9(04).
      *
           03  ORCHC30-DATA-TBL        OCCURS   99.
      *        ���ޥ�����
               05  ORCHC30-SRYCD       PIC X(09).
      *        ��ˡ�����ɡʶ��򤢤��
               05  ORCHC30-YSRYCD      PIC X(09).
      *        ��ˡ������̾��
      *R02.4   05  ORCHC30-YSRYCD-NAME PIC X(80).
               05  ORCHC30-YSRYCD-NAME PIC X(140).
      *
      *        ���̡ʣ����̡����Ѥ����̡�
               05  ORCHC30-SURYO       PIC 9(05)V9(05).
      *        �����ʲ���ˡʳ��Ѥϣ����
               05  ORCHC30-KAISU       PIC 9(03).
      *        �ʣ��������������������������ѡ����������
               05  ORCHC30-ZAIKBN      PIC X(01).
      *        ��ˡ������̡ʣ���ī�������롢�����롢������������������
               05  ORCHC30-YOURYOU-TBL.
                   07  ORCHC30-YOURYOU     PIC 9(05)V9(05)
                                           OCCURS   5.
      *        �����������������Ϥ����ä����Τߡ�
               05  ORCHC30-SRYKAISU        PIC 9(03).
      *        �����Ϳ��̡ʣ����̡�
               05  ORCHC30-KANSURYO        PIC 9(05)V9(05).
      *H28.10
      *        ��ˡ�����ɤο���
      *           �ʣ���ī�������롢�����롢������������������
               05  ORCHC30-INPUTSURYOU-TBL.
                   07  ORCHC30-INPUTSURYOU     PIC 9(05)V9(05)
                                               OCCURS   5.
      *        ��ˡ�����ɣ���ɽ���Τߡ�
               05  ORCHC30-YOHOU-G.
                   07  ORCHC30-YOHOU-TBL   OCCURS  5.
                       09  ORCHC30-YOHOU       PIC X(09).
      *                ��ˡ������̾��
      *R02.4           09  ORCHC30-YOHOU-NAME  PIC X(80).
                       09  ORCHC30-YOHOU-NAME  PIC X(140).
      *        �����ȥ�����
               05  ORCHC30-COMMENT-G.
                   07  ORCHC30-COMMENT-OCCURS  OCCURS  10.
                       09  ORCHC30-COMMENT-CD   PIC X(09).
      *R02.4           09  ORCHC30-COMMENT-MEI  PIC X(80).
                       09  ORCHC30-COMMENT-MEI  PIC X(140).
      *H28.10
      *                1:��ˡ�����ɤο����Խ�ʬ
                       09  ORCHC30-COMHENKBN    PIC X(01).
      *