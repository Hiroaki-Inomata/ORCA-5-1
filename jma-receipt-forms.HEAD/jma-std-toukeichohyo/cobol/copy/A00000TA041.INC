      ******************************************************************
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
      *  �V�X�e����        : �n�q�b�`
      *  �T�u�V�X�e����    : �������[
      *  �R���|�[�l���g��  : ���@�E�݉@�󋵕��͕\
      *  �Ǘ���            : 
      *  Maj/Min/Rev  �C����  ���t        ���e
      *  01.00.00     �c��    2011/03/23  �V�K�쐬
      ******************************************************************
       01  TA041.
      *    �f�ÔN��
           03  TA041-SRYYM           PIC X(16).
      *    �O���[�v�^�C�g��
           03  TA041-GRPTITLE        PIC X(20).
      *    �쐬�N����
           03  TA041-CREATEYMD       PIC X(22).
      *    �Ő�
           03  TA041-PAGE            PIC X(02).
      *    ���v�Ő�
           03  TA041-GPAGE           PIC X(02).
      *    ���O�敪
           03  TA041-NYUGAI          PIC X(04).
      *    ���׃e�[�u��
           03  TA041-MEISAI-TBL      OCCURS 7 TIMES.
      *    �Ȗ��^��t��
             05  TA041-KANAME        PIC X(20).
      *    �J�z�݉@��
             05  TA041-KZAIIN        PIC X(05).
      *    ���@��
             05  TA041-NYUIN         PIC X(05).
      *    �މ@��
             05  TA041-TAIIN         PIC X(05).
      *    [��]���S
             05  TA041-SIBOU         PIC X(05).
      *    �݉@��
             05  TA041-ZAIIN         PIC X(05).
      *    �݉@���א�
             05  TA041-NZAIIN        PIC X(05).
      *    [��]�O�����א�
             05  TA041-NGAIHAKU      PIC X(05).
      *    �P�����ϓ��@��
             05  TA041-NYUIN-AVG     PIC X(05).
      *    �P�����ϑމ@��
             05  TA041-TAIIN-AVG     PIC X(05).
      *    �P�����ύ݉@��
             05  TA041-ZAIIN-AVG     PIC X(05).
      *    ���v�e�[�u��
           03  TA041-GOKEI-TBL.
      *    ���v�\�J�z�݉@��
             05  TA041-GKZAIIN       PIC X(06).
      *    ���v�\���@��
             05  TA041-GNYUIN        PIC X(06).
      *    ���v�\�މ@��
             05  TA041-GTAIIN        PIC X(06).
      *    ���v�\[��]���S
             05  TA041-GSIBOU        PIC X(06).
      *    ���v�\�݉@��
             05  TA041-GZAIIN        PIC X(06).
      *    ���v�\�݉@���א�
             05  TA041-GNZAIIN       PIC X(06).
      *    ���v�\[��]�O�����א�
             05  TA041-GNGAIHAKU     PIC X(06).
      *    ���v�\�P�����ϓ��@��
             05  TA041-GNYUIN-AVG    PIC X(06).
      *    ���v�\�P�����ψ�����
             05  TA041-GTAIIN-AVG    PIC X(06).
      *    ���v�\�P�����ύ݉@��
             05  TA041-GZAIIN-AVG    PIC X(06).