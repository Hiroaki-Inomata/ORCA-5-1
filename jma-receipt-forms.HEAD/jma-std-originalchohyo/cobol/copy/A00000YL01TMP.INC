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
       01  YL01TMP.
           02  YL01TMP-KEY.
      *      �D�揇�ʁi�P�`�T�j
             03  YL01TMP-KEY-TBL      OCCURS 5.
               04  YL01TMP-KEY-D        PIC X(64).
      *      ���i��
             03  YL01TMP-NAME         PIC X(64).
      *      �f�Ís�׃R�[�h
             03  YL01TMP-SRYCD        PIC X(9).
      *    ���̓R�[�h
           02  YL01TMP-INPUTCD      PIC X(10).
      *    ��܋敪
           02  YL01TMP-YKZKBN       PIC X(2).
      *    ��ʖ��L�ڋ敪
           02  YL01TMP-KISAIKBN     PIC X(2).
      *    ��ʖ�
           02  YL01TMP-GNAME        PIC X(64).
      *    ��
           02  YL01TMP-YAKKA        PIC X(9).
      *    �Œ��
           02  YL01TMP-SAIYAKKA     PIC X(9).
