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
      *-------------------------------------------------*
      *    �ѥ���ɺ�����ǧ�ڥѥ�᥿  COPY (CPORCSAUTH.INC)     *
      *-------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *
       01  ORCSAUTHAREA.
      *���ޥ�ɡ�"passwd" or "auth")
           03  AUTH-COMMAND            PIC X(08).
      *�Ź沽��ˡ��01:MD5��
           03  AUTH-ID                 PIC 9(02).
      *�ѥ���ɡʥץ졼���
           03  AUTH-PASSWORD1          PIC X(128).
      *�Ź沽�ѥ����
           03  AUTH-PASSWORD2          PIC X(128).
      *��̡�0:���� 1:���ԡ�
           03  AUTH-RC                 PIC 9(01).
      *