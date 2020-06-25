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
      *    �ե�����¸�ߥ����å�����(C�ؿ��Ȥ�Ϣ���ΰ�)
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  CACCESS-AREA.
      *    1024����
           03  CACCESS-STR-LEN                 PIC S9(9) BINARY.
      *    0:F_OK/1:X_OK/2:W_OK/4:R_OK
           03  CACCESS-MODE                    PIC S9(9) BINARY.
      *    �Ķ��ѿ�(COB_LIBRARY_PATH��)
           03  CACCESS-ENVNAME                 PIC X(1024).
      *    �ե�����̾�����ϡ�
      *      �̾�ϥѥ��դ������ꤹ�롣
      *      MODE=X_OK�λ��˥ե�����̾�Τߤǻ��ꤵ��Ƥ����硢
      *      �Ķ��ѿ��򻲾Ȥ��ƥե������õ�����롣
           03  CACCESS-IN-FILENAME             PIC X(1024).
      *----�ʲ��ֵѹ���
      *    �ե�ѥ��դ��ե�����̾
           03  CACCESS-FULLPATH                PIC X(1024).
      *    �ե������¸�ߤ���ǥ��쥯�ȥ�̾
           03  CACCESS-OT-DIRNAME              PIC X(1024).
      *    �ѥ��ʤ��ե�����̾
           03  CACCESS-OT-FILENAME             PIC X(1024).
      *    �񤭹��߲�ǽ��
      *    (1:ͭ/0:̵��
           03  CACCESS-WRITABLE                PIC S9(9) BINARY.
      *    �ե����뤬¸�ߤ��뤫
      *    (1:ͭ/0:̵��
           03  CACCESS-FILE-EXISTS             PIC S9(9) BINARY.
      *    �¹Է�� (0:õ������ / 0�ʳ�: õ������)
           03  CACCESS-RC                      PIC S9(9) BINARY.