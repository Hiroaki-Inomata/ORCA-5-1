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
      *
      *     �����ѥǡ��� ̾���ΰ�      CPCOMMONPRT.INC
      *            
      *****************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    MCC-ƣ��     02/02/04  �������֤��ɲ�
      *                                     Ϣ��X(03)����9(06)���ѹ�
      *  01.00.02    MCC-ƣ��     02/03/18  ü���ɣ�
      *                                     X(16)����X(32)���ѹ�
      *  03.05.00    NACL-����    07/05/24  ���롼�׿����б�
      *****************************************************************
       01  HC01PARA.
           03  FILLER                      PIC  X(05) VALUE "/tmp/".
           03  HC01PARA-BASENAME.
               05  HC01PARA-HOSPNUM        PIC  9(02).
               05  HC01PARA-FORM-ID        PIC  X(04).
               05  FILLER                  PIC  X(01) VALUE ".".
               05  HC01PARA-TERMID         PIC  X(32).
               05  FILLER                  PIC  X(01) VALUE ".".
               05  HC01PARA-SYOHMS         PIC  9(08).
               05  FILLER                  PIC  X(01) VALUE ".".
               05  HC01PARA-CNT            PIC  9(06).
               05  FILLER                  PIC  X(04) VALUE ".dat".