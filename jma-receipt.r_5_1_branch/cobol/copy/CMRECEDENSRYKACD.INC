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
      *----------------------------------------------------------------*
      *    �쥻�ſ��Ųʥ����ɥơ��֥�   COPY (CMRECEDENSRYKACD.INC)      *
      *----------------------------------------------------------------*
      *****************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  03.03.01    NACL-ƣ��    06/10/16  ������ʤ˽���
      *
      *  04.05.01    NACL-ƣ��    10/03/23  ʿ������ǯ��������б�
      *****************************************************************
      *
      *    �쥻�ſ��Ųʥ����ɥơ��֥�
       01  TBL-RECEDENSRYKACD-AREA.
               05  FILLER          PIC X(02)   VALUE   "01". 
               05  FILLER          PIC X(20)   VALUE   "���".
               05  FILLER          PIC X(02)   VALUE   "02". 
               05  FILLER          PIC X(20)   VALUE   "������".
               05  FILLER          PIC X(02)   VALUE   "03". 
               05  FILLER          PIC X(20)   VALUE   "���в�".
               05  FILLER          PIC X(02)   VALUE   "04". 
               05  FILLER          PIC X(20)   VALUE   "�������".
               05  FILLER          PIC X(02)   VALUE   "05". 
               05  FILLER          PIC X(20)   VALUE   "�Ƶ۴��".
               05  FILLER          PIC X(02)   VALUE   "06". 
               05  FILLER          PIC X(20)   VALUE   "�ò����".
               05  FILLER          PIC X(02)   VALUE   "07". 
               05  FILLER          PIC X(20)   VALUE   "��Ĳ��".
               05  FILLER          PIC X(02)   VALUE   "08". 
               05  FILLER          PIC X(20)   VALUE   "�۴Ĵ��".
               05  FILLER          PIC X(02)   VALUE   "09". 
               05  FILLER          PIC X(20)   VALUE   "������".
               05  FILLER          PIC X(02)   VALUE   "10". 
               05  FILLER          PIC X(20)   VALUE   "����".
               05  FILLER          PIC X(02)   VALUE   "11". 
               05  FILLER          PIC X(20)   VALUE   "��������".
               05  FILLER          PIC X(02)   VALUE   "12". 
               05  FILLER          PIC X(20)   VALUE   "��������".
               05  FILLER          PIC X(02)   VALUE   "13". 
               05  FILLER          PIC X(20)   VALUE   "���Ƴ���".
               05  FILLER          PIC X(02)   VALUE   "14". 
               05  FILLER          PIC X(20)   VALUE   "Ǿ���г���".
               05  FILLER          PIC X(02)   VALUE   "15". 
               05  FILLER          PIC X(20)   VALUE   "�Ƶ۴ﳰ��".
               05  FILLER          PIC X(02)   VALUE   "16". 
               05  FILLER          PIC X(20)   VALUE   "��¡��ɳ���".
               05  FILLER          PIC X(02)   VALUE   "17". 
               05  FILLER          PIC X(20)   VALUE   "��������".
               05  FILLER          PIC X(02)   VALUE   "18". 
               05  FILLER          PIC X(20)   VALUE   "������Ǣ���".
               05  FILLER          PIC X(02)   VALUE   "19". 
               05  FILLER          PIC X(20)   VALUE   "�����".
               05  FILLER          PIC X(02)   VALUE   "20". 
               05  FILLER          PIC X(20)   VALUE   "��Ǣ���".
               05  FILLER          PIC X(02)   VALUE   "21". 
               05  FILLER          PIC X(20)   VALUE   "���²�".
               05  FILLER          PIC X(02)   VALUE   "22". 
               05  FILLER          PIC X(20)   VALUE   "�������".
               05  FILLER          PIC X(02)   VALUE   "23". 
               05  FILLER          PIC X(20)   VALUE   "���ؿͲ�".
               05  FILLER          PIC X(02)   VALUE   "24". 
               05  FILLER          PIC X(20)   VALUE   "����".
               05  FILLER          PIC X(02)   VALUE   "25". 
               05  FILLER          PIC X(20)   VALUE   "�ؿͲ�".
               05  FILLER          PIC X(02)   VALUE   "26". 
               05  FILLER          PIC X(20)   VALUE   "���".
               05  FILLER          PIC X(02)   VALUE   "27". 
               05  FILLER          PIC X(20)   VALUE   "��ɡ���󤳤���".
               05  FILLER          PIC X(02)   VALUE   "28". 
               05  FILLER          PIC X(20)   VALUE   "���ɿ�ƻ��".
               05  FILLER          PIC X(02)   VALUE   "30". 
               05  FILLER          PIC X(20)   VALUE   "��������".
               05  FILLER          PIC X(02)   VALUE   "31". 
               05  FILLER          PIC X(20)   VALUE   "����".
               05  FILLER          PIC X(02)   VALUE   "33". 
               05  FILLER          PIC X(20)   VALUE   "�������".
               05  FILLER          PIC X(02)   VALUE   "34". 
               05  FILLER          PIC X(20)   VALUE   "����륮����".
               05  FILLER          PIC X(02)   VALUE   "35". 
               05  FILLER          PIC X(20)   VALUE   "�ꥦ�ޥ���".
               05  FILLER          PIC X(02)   VALUE   "36". 
               05  FILLER          PIC X(20)   VALUE
                                               "��ϥӥ�ơ�������".
               05  FILLER          PIC X(02)   VALUE   "37". 
               05  FILLER          PIC X(20)   VALUE   "�������ǲ�".
               05  FILLER          PIC X(02)   VALUE   "38". 
               05  FILLER          PIC X(20)   VALUE   "�׾�������".
               05  FILLER          PIC X(02)   VALUE   "39". 
               05  FILLER          PIC X(20)   VALUE   "�ߵ޲�".
      *
       01  TBL-RECEDENSRYKACD-AREA-R   REDEFINES
                                               TBL-RECEDENSRYKACD-AREA.
           03  TBL-RECEDENSRYKACD-OCC  OCCURS      37
                                       INDEXED TBL-RECEDENSRYKACD-IDX.
               05  TBL-RECEDEN-SRYKACD
                                   PIC X(02).
               05  TBL-RECEDEN-SRYKANAME
                                   PIC X(20).
      *
       01  TBL-RECEDENSRYKACD-MAX  PIC 9(02)   VALUE   37.

