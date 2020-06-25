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
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       01  FMMNOTE-REC.
           03  FMMNOTE-CONTEXT                     PIC S9(9)   BINARY.
           03  FMMNOTE-OBJECT                      PIC X(36).
           03  FMMNOTE-MODE                        PIC S9(9)   BINARY.
           03  FMMNOTE-RECORDNAME                  PIC X(128).
           03  FMMNOTE-DATA.
      *            ���[�h�c
               05  FMMNOTE-FORM-ID                PIC X(30).
      *            �J�X�^���h�c
               05  FMMNOTE-CUSTOM-ID               PIC X(30).
      *            �v�����^��
               05  FMMNOTE-PRINTER                 PIC X(20).
      *            �f�Ó� YYYY-MM-DD
               05  FMMNOTE-SRYYMD                  PIC X(10).
      *            �`�[�ԍ�
               05  FMMNOTE-DENPNUM                 PIC X(7).
      *            ���ҏ��
               05  FMMNOTE-PTINF-AREA.
      *                ���Ҕԍ�
                   07  FMMNOTE-PTNUM               PIC X(20).
      *                ����
                   07  FMMNOTE-NAME                PIC X(50).
      *                �J�i����
                   07  FMMNOTE-KANANAME            PIC X(50).
      *                �a����
                   07  FMMNOTE-BIRTHDAY            PIC X(10).
      *                ���ʋ敪
                   07  FMMNOTE-SEX                 PIC X(1).
      *            ��Ë@�֏��
               05  FMMNOTE-HOSP-AREA.
      *                ����
                   07  FMMNOTE-PREFNUM             PIC X(2).
      *                ����
                   07  FMMNOTE-HOSP-NAME-G.
                       09  FMMNOTE-HOSP-NAME       PIC X(120)
                                                   OCCURS   3.
      *                �Z��
                   07  FMMNOTE-HOSP-ADRS-G.
                       09  FMMNOTE-HOSP-ADRS       PIC X(200)
                                                   OCCURS   3.
      *                �d�b�ԍ�
                   07  FMMNOTE-HOSP-TEL            PIC X(15).
               05  FMMNOTE-DR-AREA.
      *                ��t�R�[�h
                   07  FMMNOTE-DR-CODE             PIC X(5).
      *                ��t��
                   07  FMMNOTE-DR-NAME             PIC X(50).
               05  FMMNOTE-ZAI-AREA                OCCURS  50 TIMES.
      *                �f�Ë敪
                   07  FMMNOTE-SRYKBN              PIC X(2).
      *                �܉�
                   07  FMMNOTE-ZAIKAISU            PIC X(3).
      *                �����_�܋敪
                   07  FMMNOTE-NAIHENKBN           PIC X(01).
      *                �ܒP�ʖ��i�񐔁^�񕪁j
                   07  FMMNOTE-ZAITANINAME         PIC X(12).
                   07  FMMNOTE-MEDICATION          OCCURS  50 TIMES.
      *                ��ܖ�
                       09  FMMNOTE-YKZNAME         PIC X(80).
      *                ����(ZZZZ9.99999)
                       09  FMMNOTE-SURYO           PIC X(12).
      *                �P�ʖ�
                       09  FMMNOTE-TANINAME        PIC X(12).
      *                �f�ÃR�[�h
                       09  FMMNOTE-SRYCD           PIC X(9).
      *        ���l
               05  FMMNOTE-MEMO    PIC X(80)       OCCURS  10 TIMES.
      *        �p�q�擾�p�h�c
               05  FMMNOTE-QR-ID   PIC X(80).
