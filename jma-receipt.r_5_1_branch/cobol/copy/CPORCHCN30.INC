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
      *    �������޾�����ϥѥ�᥿          COPY (CPORCHCN30.INC)
      *                                          (ORCHCN30 �ǻ��ѡ�
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 04.05.00     NACL-����    09/06/22  �������ʲ�����
      * 04.05.00     NACL-¿��Ǽ  09/07/23  �������ޡ��������ɲ�
      * 04.06.00     NACL-¿��Ǽ  11/02/22  ��ˡ�������б��ɲ�
      * 04.08.00     NACL-¿��Ǽ  16/11/XX  ��ˡ�����ɿ����ɲ�
      * 05.00.00     NACL-¿��Ǽ  17/05/16  �����ֹ��ɲ�
      ************************************************************
      *
       01  ORCHCN30AREA.
      *    �ɥ�����
           03  ORCHCN30-DRCD            PIC X(05).
      *    ����
           03  ORCHCN30-BTUNAME         PIC X(20).
      *    �¼�
           03  ORCHCN30-ROOMNO          PIC X(06).
      *H29.5
      *    �����ֹ�
           03  ORCHCN30-BTUNUM          PIC X(02).
      *    ������
           03  ORCHCN30-MAX             PIC 9(04).
      *
           03  ORCHCN30-DATA-TBL        OCCURS   99.
      *        ���ޥ�����
               05  ORCHCN30-SRYCD       PIC X(09).
      *        ��ˡ�����ɡʶ��򤢤��
               05  ORCHCN30-YSRYCD      PIC X(09).
      *        ��ˡ������̾��
      *R02.4   05  ORCHCN30-YSRYCD-NAME PIC X(80).
               05  ORCHCN30-YSRYCD-NAME PIC X(140).
      *        ���̡ʣ����̡����Ѥ����̡�
               05  ORCHCN30-SURYO       PIC 9(05)V9(05).
      *        �����ʲ���ˡʳ��Ѥϣ����
               05  ORCHCN30-KAISU       PIC 9(03).
      *        �ʣ��������������������������ѡ����������
               05  ORCHCN30-ZAIKBN      PIC X(01).
      *        ��ˡ������̡ʣ���ī�������롢�����롢������������������
               05  ORCHCN30-YOURYOU-TBL.
                   07  ORCHCN30-YOURYOU     PIC 9(05)V9(05)
                                           OCCURS   5.
      *        �����������������Ϥ����ä����Τߡ�
               05  ORCHCN30-SRYKAISU        PIC 9(03).
      *        �����Ϳ��̡ʣ����̡�
               05  ORCHCN30-KANSURYO    PIC 9(05)V9(05).
      *H28.11
      *        ��ˡ�����ɤο���
      *           �ʣ���ī�������롢�����롢������������������
               05  ORCHCN30-INPUTSURYOU-TBL.
                   07  ORCHCN30-INPUTSURYOU    PIC 9(05)V9(05)
                                               OCCURS   5.
      *        ��ˡ�����ɣ���ɽ���Τߡ�
               05  ORCHCN30-YOHOU-G.
                   07  ORCHCN30-YOHOU-TBL      OCCURS  5.
                       09  ORCHCN30-YOHOU       PIC X(09).
      *                ��ˡ������̾��
      *R02.4           09  ORCHCN30-YOHOU-NAME  PIC X(80).
                       09  ORCHCN30-YOHOU-NAME  PIC X(140).
      *        �����ȥ�����
               05  ORCHCN30-COMMENT-G.
                   07  ORCHCN30-COMMENT-OCCURS  OCCURS  10.
                       09  ORCHCN30-COMMENT-CD   PIC X(09).
      *R02.4           09  ORCHCN30-COMMENT-MEI  PIC X(80).
                       09  ORCHCN30-COMMENT-MEI  PIC X(140).
      *H28.10
      *                1:��ˡ�����ɤο����Խ�ʬ
                       09  ORCHCN30-COMHENKBN    PIC X(01).
      *