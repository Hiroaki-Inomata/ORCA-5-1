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
      *    ����ե�����̾�������֡�COPY (CPORCSGETTEMP.INC)
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  SGETTEMP-AREA.
      *    �ߥɥ륦����
           03  SGETTEMP-MW                 PIC 9(01).
      *    ���ϰ���
      *    �ե�����̾�ʥѥ�̵����
           03  SGETTEMP-BASENAME           PIC X(1024).
           03  SGETTEMP-BASENAMES-OCC      OCCURS 10.
               05  SGETTEMP-BASENAMES      PIC X(1024).
      *    �ֵ���
      *    FULLNAME���BASENAME���ϰ���
           03  SGETTEMP-ST                 PIC 9(05).
      *    ����ǥ��쥯�ȥ�̾
           03  SGETTEMP-TEMPDIR            PIC X(1024).
           03  SGETTEMP-FULLNAME-AREA.
      *    TEMPDIR + "/" + BASENAME
               05  SGETTEMP-FULLNAME       PIC X(1024).
               05  SGETTEMP-FULLNAMES-OCC  OCCURS 10.
                   07  SGETTEMP-FULLNAMES  PIC X(1024).