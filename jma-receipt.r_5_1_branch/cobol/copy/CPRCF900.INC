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
      *    �ģУ�Ĵ���б�     ����ֹ���� (CPRCF900.INC)      *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.00.01    NACL-��Ʀ��  xx/xx/xx  ���롼�׿����б�
      ************************************************************
      *01  RECE900-REC.
           03  RECE900-KEY.
      *���ŵ��أɣ�
               05  RECE900-HOSPNUM         PIC  9(02).
      *����ǯ��
               05  RECE900-SRYYM           PIC  9(06).
      *���ԣɣ�
               05  RECE900-PTID            PIC  9(10).
      *���Ŷ�ʬ        
               05  RECE900-SRYKBN          PIC  X(02).
      *����ֹ�        
           03  RECE900-RENNUM              PIC  9(04).
