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
000100************************************************************
000200* �����ģ��Ѿ����ΰ�      CPORCSPRTLNK.INC                 *
000300************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-ƣ��    03/03/10  ü���ɣĤ򣶣����ѹ�
      ************************************************************
           03  LNK-PRTKANRI-AREA.
      *    Ϣ��
               05  LNK-PRTKANRI-RENNUM    PIC 9(04).
      *    �ˣţ�
               05  LNK-PRTKANRI-TBL-KEY   PIC X(08).
      *    ���롼��
               05  LNK-PRTKANRI-TBL-GROUP PIC X(14).
      *    UUID
               05  LNK-PRTKANRI-TBL-UUID  PIC X(36).
      *    ������Ϣ��
               05  LNK-PRTKANRI-SHORI-RENNUM
                                          PIC 9(04).
      *    ��̳������
               05  LNK-PRTKANRI-GYOUMU-CD
                                          PIC 9(03).
      *    ����ǯ��
               05  LNK-PRTKANRI-SRYYM     PIC X(06).
      *    ����ǯ����
               05  LNK-PRTKANRI-SKYYMD    PIC X(08).
      *    ������ɣ�
               05  LNK-PRTKANRI-SHELLID   PIC X(20).
      *    ������Ϣ��
               05  LNK-PRTKANRI-PRIORITY  PIC 9(04).
      *    ü���ɣ�
               05  LNK-PRTKANRI-TERMID    PIC X(64).
      *    ���ڥ졼���ɣ�
               05  LNK-PRTKANRI-OPID      PIC X(16).
      *    �ץ��̾
               05  LNK-PRTKANRI-PRTNM     PIC X(16).
