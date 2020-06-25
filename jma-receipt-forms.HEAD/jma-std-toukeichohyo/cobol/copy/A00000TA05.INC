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
      *  �V�X�e����        : �n�q�b�`
      *  �T�u�V�X�e����    : �������[
      *  �R���|�[�l���g��  : �w��a���E�f�Ís�׊��҈ꗗ
      *  �Ǘ���            : 
      *  Maj/Min/Rev  �C����  ���t        ���e
      *   01.00.00    �c��    2011/03/07  �V�K�쐬
      *****************************************************************
       01  TA05.
      *    �f�ÑΏۊJ�n��
           03  TA05-SRYSTYMD          PIC X(22).
      *    �f�ÑΏۏI����
           03  TA05-SRYEDMD           PIC X(12).
      *    ���O�敪
           03  TA05-NYUGAIKBN         PIC X(10).
      *    �Ő�
           03  TA05-PAGE              PIC X(03).
      *    ���v�Ő�
           03  TA05-GPAGE             PIC X(03).
      *    ���s��
           03  TA05-PRTYMD            PIC X(22).
      *    �����Ώېf�Ís�ׁE�a��
           03  TA05-SEARCHNAME        PIC X(160).
      *
           03  TA05-NAIYO-TBL         OCCURS  22 TIMES.
      *    �A��
             05  TA05-RENNUM          PIC X(03).
      *    ���Ҕԍ�
             05  TA05-PTNUM           PIC X(10).
      *    ���Ҕԍ�(11���ȏ�)
             05  TA05-PTNUM2          PIC X(20).
      *    �J�i����
             05  TA05-KANA-NAME       PIC X(30).
      *    ����
             05  TA05-NAME            PIC X(22).
      *    ����
             05  TA05-SEX             PIC X(02).
      *    �N��
             05  TA05-AGE             PIC X(03).
      *    �a��
             05  TA05-BYOMEI          PIC X(22).
      *    �a��(12�����ȏ�)
             05  TA05-BYOMEI2         PIC X(44).
      *    �a���J�n��
             05  TA05-BYOSTYMD        PIC X(18).
      *    �]�A��
             05  TA05-TENKIYMD        PIC X(18).
      *    �]�A
             05  TA05-TENKI           PIC X(04).
      *    �f�Ís�ז�
             05  TA05-SRYNAME         PIC X(22).
      *    �f�Ís�ז�(12�����ȏ�)
             05  TA05-SRYNAME2        PIC X(44).
      *    �ŏI���@��
             05  TA05-LASTYMD         PIC X(18).