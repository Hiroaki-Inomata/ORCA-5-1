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
      *    �ѥ����        COPY   (CPPASSWD.INC) *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������        ����      ����
      ************************************************************
       01  PASSWD-REC.
      *����
           03  PASSWD-KEY.
      *�桼�����ɣ�
               05  PASSWD-USERID     PIC X(16).
      *�ѥ����
           03  PASSWD-PASSWORD       PIC X(256).
      *�ʲ� virtual
      *    �桼���ɣġʹ������
           03  PASSWD-UPD-USERID     PIC X(16).