      *******************************************************************
      * Project code name "ORCA"
      * ����W�����Z�v�g�\�t�g�iJMA standard receipt software�j
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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ******************************************************************
      *  Maj/Min/Rev  �C����  ���t       ���e
      *   01.00.01    ���    12/05/01   �V�K�쐬
      ******************************************************************
       01  YL01.
      *    �D�揇�ʁi�P�`�T�j
           02  YL01-TITLE1     PIC X(90).
      *    �p�����^�ݒ�i�U�`�X�j
           02  YL01-TITLE2     PIC X(100).
      *    �쐬�N����
           02  YL01-PRTYMD     PIC X(22).
      *    �Ő�
           02  YL01-PAGE       PIC X(4).
      *    ���v�Ő�
           02  YL01-GPAGE      PIC X(4).
      *
           02  YL01-TBL        OCCURS  34 TIMES.
      *      �f�Ís�׃R�[�h
             03  YL01-SRYCD      PIC X(9).
      *      ���̓R�[�h
             03  YL01-INPUTCD    PIC X(10).
      *      �敪
             03  YL01-YKZKBN     PIC X(2).
      *      �y�ʁz
             03  YL01-KISAIKBN   PIC X(2).
      *      ���i��
             03  YL01-NAME       PIC X(64).
      *      ��ʖ�
             03  YL01-GNAME      PIC X(64).
      *      ��
             03  YL01-YAKKA      PIC X(9).
      *      �Œ��
             03  YL01-SAIYAKKA   PIC X(9).