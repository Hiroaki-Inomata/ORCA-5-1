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
      *     �쥻�ץ����٥ե����� ̾���ΰ�      CPCOMMONDAT2.INC
      *     
      *     ���������ǻ��Ѥ��Ƥ���ΤǣԣţңͣɣĤ�Ĺ�����ѹ��Ϥ��ʤ� 
      *            
      *****************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  03.05.01    NACL-ƣ��    07/04/19  ���롼�׿����б�
      *****************************************************************
       01  RECE01PARA-AREA.
           03  RECE01PARA.
               05  RECE01PARA-OLD.
                   07  FILLER                  PIC X(05) VALUE "/tmp/".
                   07  RECE01PARA-BASENAME.
                       09  RECE01PARA-HOSPNUM  PIC 9(02).
                       09  RECE01PARA-FILE-ID  PIC X(07).
                       09  RECE01PARA-TERMID   PIC X(16).
                   07  FILLER                  PIC X(994) VALUE SPACE.
               05  RECE01PARA-FULLNAME-R REDEFINES RECE01PARA-OLD.
                   07  RECE01PARA-FULLNAME     PIC X(1024).