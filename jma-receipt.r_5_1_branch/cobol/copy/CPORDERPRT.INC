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
      *****************************************************************
      *    ����Ģɼ������      COPY (CPORDERPRT.INC)                 *
      *****************************************************************
      *****************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 05.00.00     NACL-¿��Ǽ  20/02/XX  �������ͷ���ѹ�
      *****************************************************************
      *01  ORDERPRT-REC.
           03  ORDERPRT-KEY.
      *  ���ŵ��أɣ�
               05  ORDERPRT-HOSPNUM        PIC 9(2).
      *  ���ԣɣ�
               05  ORDERPRT-PTID          PIC  9(10).
      *  �����ֹ�
               05  ORDERPRT-PTNUM         PIC  X(20).
      *  ������ʬ
               05  ORDERPRT-NYUGAIKBN     PIC  X(01).
      *  ���Ų�
               05  ORDERPRT-SRYKA         PIC  X(02).
      *  �ݸ��ȹ礻
               05  ORDERPRT-HKNCOMBI      PIC  X(04).
      *  �����ƥ�����
               05  ORDERPRT-SYSYMD        PIC  X(08).
      *  ȯ����
               05  ORDERPRT-PRINTYMD      PIC X(08).
      *  ȯ�Ի���
               05  ORDERPRT-PRINTHMS      PIC X(06).
      *  Ģɼ����  ������������䵡������������������������׻�����
      *            �������ͽ���䵡������������������������ؼ��
               05  ORDERPRT-PRTKBN        PIC  X(01).
      *  �ֹ�
               05  ORDERPRT-ORDERNUM      PIC 9(02).
      *  ���֡ʺ���ˣ������Ϣ�֡�
               05  ORDERPRT-RENNUM        PIC 9(02).
      *Ʊ���ֹ�Ϥ��٤�Ʊ������
      *  �ɥ�����������
           03  ORDERPRT-DRCD              PIC X(05).
      *  ���Ŷ�ʬ
           03  ORDERPRT-SRYKBN            PIC X(02).
      *  ���ż���
           03  ORDERPRT-SRYSYUKBN         PIC X(03).
      *  ��������
           03  ORDERPRT-SRYYMD            PIC X(08).
      *���֡ᣰ���λ�����
      *  ���
           03  ORDERPRT-DAY-G.
               05  ORDERPRT-DAY           PIC 9(03)
                                       OCCURS   31.
      *�������ơ�ɬ�ܡ�
           03  ORDERPRT-SINRYO-G.
      *  ���ť�����
               05  ORDERPRT-SRYCD             PIC X(09).
      *  ����
               05  ORDERPRT-SRYSURYO          PIC 9(05)V9(5).
      *  ���̡ʳ��Ѥθ��̿��̡�
               05  ORDERPRT-SRYSURYO2         PIC 9(05)V9(5).
      *  ���Ʋ�������Ѥθ��̲��
               05  ORDERPRT-SRYKAISU          PIC 9(03).
      *  ���ϥ����ȡʥե꡼����������
      *R02.4   05  ORDERPRT-INPUTCOMENT       PIC X(80).
               05  ORDERPRT-INPUTCOMENT       PIC X(140).
      *  �����Ϳ���
               05  ORDERPRT-KANSURYO          PIC 9(05)V9(5).
      *
      *ü���ɣ�
           03  ORDERPRT-TERMID            PIC  X(64).
      *���ڡ��졼���ɣ�
           03  ORDERPRT-OPID              PIC  X(16).
      *����ǯ����
           03  ORDERPRT-CREYMD            PIC  X(08).
      *����ǯ����
           03  ORDERPRT-UPYMD             PIC  X(08).
      *��������
           03  ORDERPRT-UPHMS             PIC  X(06).
