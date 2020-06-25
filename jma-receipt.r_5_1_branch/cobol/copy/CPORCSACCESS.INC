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
      *    �ե�����¸�ߥ����å�����(COBOL)
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  SACCESS-AREA.
      *    R:�ե�����¸�ߥ����å�
      *    W:�񤭹��߲��ݥ����å�
      *    X:�¹ԥե�����¸�ߥ����å�(COB_LIBRARY_PATH�򻲾Ȥ���õ��)
           03  SACCESS-MODE                    PIC X(01).
           03  SACCESS-IN-FILENAME             PIC X(1024).
           03  SACCESS-OT-FILENAME             PIC X(1024).
           03  SACCESS-FULLPATH                PIC X(1024).
           03  SACCESS-DIRNAME                 PIC X(1024).
           03  SACCESS-EXISTS                  PIC S9(9).
           03  SACCESS-RC                      PIC S9(9).