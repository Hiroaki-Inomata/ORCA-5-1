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
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  BI201.
      *    ������ʬ��0:�ãӣֽ��ϡ�1:Ģɼ������
           03  BI201-PROC-FLG          PIC 9(01).
      *    �ãӣ֥ե�����̾
           03  BI201-CSVFILENAME       PIC X(100).
      *    ���ϥڡ�����
           03  BI201-STPAGE            PIC 9(09).
      *    ��λ�ڡ�����
           03  BI201-EDPAGE            PIC 9(09).
      *    ���Ϸ��
           03  BI201-STKNS             PIC 9(09).
      *    ��λ���
           03  BI201-EDKNS             PIC 9(09).
      *    �����
           03  BI201-KJYNYMD           PIC 9(08).
      *    SQL
           03  BI201-SQL               PIC X(15000).
