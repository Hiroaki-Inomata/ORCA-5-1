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
      *    �����ȥ����ɸ�������  COPY (CPORCSSRCCOMMCD.INC)          *
      *****************************************************************
      *****************************************************************
      * ���ԡ��罤������
      * Maj/Min/Rev    ������      ����        ����
      *****************************************************************
       01  ORCSSRCCOMMCD-AREA.
      *
      *����
      *    �����ȼ���� 1:������ˡ 2:������������ 8:������ )
           03  SRCCOM-COMMENT-KBN  PIC 9(01).
      *    �ޥ�������
           03  SRCCOM-MSTYMD       PIC X(08).
      *    ����������
           03  SRCCOM-COMMENT      PIC X(100).
      *
      *�����
      *    �����ȥ�����
           03  SRCCOM-COMMENT-CODE PIC X(09).
      *    ���������� ( 0:���������ɤ��� 1:���������ɤʤ� 2:�����ȼ������� )
           03  SRCCOM-RC           PIC 9(01).