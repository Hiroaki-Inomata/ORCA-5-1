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
      *    ����ץǡ�������̾�����     COPY (CPTCF003.INC)    *
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-ƣ��    04/02/01  ��̾�����ɣ������ѹ�
      *                                     ��̾�����ɿ����ʣ����ˤ��ѹ�
      *
      *  03.05.01    NACL-ƣ��    07/04/19  ���롼�׿����б�
      *
      *  04.06.01    NACL-ƣ��    12/10/26  �õ����ࣰ����Ϸʻ�˵ڤ�
      *                                     ������Ϸ��ˤΥ쥻�ץ��б�
      ************************************************************
      *01  TOUKEI03-REC.
           03  TOUKEI03-KEY.
      *        ���ŵ��أɣ�
               05  TOUKEI03-HOSPNUM        PIC 9(2).
      *        ����ǯ��
               05  TOUKEI03-SRYYM          PIC  9(06).
      *        ������ʬ
               05  TOUKEI03-NYUGAIKBN      PIC  X(01).
      *        ���ԣɣ�
               05  TOUKEI03-PTID           PIC  9(10).
      *        �������ʬ
               05  TOUKEI03-RECEIPT-KBN    PIC  X(10).
      *        �쥻�ץȿ��Ų�
               05  TOUKEI03-RECEKA         PIC  X(02).
      *        ���Ų�
               05  TOUKEI03-SRYKA          PIC X(02).
      *        ���ų�����
               05  TOUKEI03-SRYYMD         PIC X(08).
      *        Ϣ�ֹ�
               05  TOUKEI03-RENNUM         PIC 9(02).
      *�쥻�ſ��Ų�
           03  TOUKEI03-RECESRYKA      PIC  X(02).
      *��̾�����ɿ�
           03  TOUKEI03-BYOMEICDSU     PIC 9(02).
      *��̾������
           03  TOUKEI03-BYOMEICD-G.
               05  TOUKEI03-BYOMEICD   PIC X(07)
                                       OCCURS   21.
      *�����ե饰
           03  TOUKEI03-UTAGAIFLG      PIC X(01).
      *����̾�ե饰
           03  TOUKEI03-SYUBYOFLG      PIC X(01).
      *�����ե饰
           03  TOUKEI03-MANSEIKBN      PIC 9(02).
      *�ݸ��ȹ礻�ֹ�
           03  TOUKEI03-HKNCOMBI       PIC 9(04).
      *�쥻�ץ�ɽ���ե饰
           03  TOUKEI03-REZEFLG        PIC X(01).
      *ž����ʬ
           03  TOUKEI03-TENKIKBN       PIC X(01).
      *ž����
           03  TOUKEI03-TENKIYMD       PIC X(08).
      *��̾
           03  TOUKEI03-BYOMEI         PIC X(160).
      *��̾ʸ����
           03  TOUKEI03-BYOMEIMOJI     PIC 9(02).
      *��̾�Խ��ե饰
           03  TOUKEI03-BYOMEIHENFLG   PIC X(01).
      *�ݸ���̾�ե饰
           03  TOUKEI03-HKNBYOMEIFLG   PIC X(01).