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
       01  TA042.
      *    �f�ÔN��
           03  TA042-SRYYM             PIC X(16).
      *    �O���[�v�^�C�g��
           03  TA042-GRPTITLE          PIC X(20).
      *    �쐬�N����
           03  TA042-CREATEYMD         PIC X(22).
      *    �Ő�
           03  TA042-PAGE              PIC X(02).
      *    ���v�Ő�
           03  TA042-GPAGE             PIC X(02).
      *    ���O�敪
           03  TA042-NYUGAI            PIC X(04).
      *    ���׃e�[�u��
           03  TA042-MEISAI-TBL        OCCURS 7 TIMES.
      *    �Ȗ��^��t��
             05  TA042-KANAME          PIC X(20).
      *    ���f���Ґ�
             05  TA042-SYOSIN          PIC X(05).
      *    [��]�V���Ґ�
             05  TA042-SINKAN          PIC X(05).
      *    [��]�f�Î��ԊO
             05  TA042-SYOJKNGAI       PIC X(05).
      *    �Đf���Ґ�
             05  TA042-SAISIN          PIC X(05).
      *    [��]�f�Î��ԊO
             05  TA042-SAIJKNGAI       PIC X(05).
      *    ���҉��א�
             05  TA042-NNINZU          PIC X(05).
      *    ���l��
             05  TA042-JNINZU          PIC X(05).
      *    �P�����ϊ��Ґ�
             05  TA042-KANJA-AVG       PIC X(05).
      *    [��]���f���ϐ�
             05  TA042-SYOSIN-AVG      PIC X(05).
      *    ���ϒʉ@��
             05  TA042-TUUIN-AVG       PIC X(05).
      *    ���v�e�[�u��
           03  TA042-GOKEI-TBL.
      *    ���v�\���f���Ґ�
             05  TA042-GSYOSIN         PIC X(06).
      *    ���v�\[��]�V���Ґ�
             05  TA042-GSINKAN         PIC X(06).
      *    ���v�\[��]�f�Î��ԊO
             05  TA042-GSYOJKNGAI      PIC X(06).
      *    ���v�\�Đf���Ґ�
             05  TA042-GSAISIN         PIC X(06).
      *    ���v�\[��]�f�Î��ԊO
             05  TA042-GSAIJKNGAI      PIC X(06).
      *    ���v�\���҉��א�
             05  TA042-GNNINZU         PIC X(06).
      *    ���v�\������
             05  TA042-GJNINZU         PIC X(06).
      *    ���v�\�P�����ϊ��Ґ�
             05  TA042-GKANJA-AVG      PIC X(06).
      *    ���v�\[��]���f���ϐ�
             05  TA042-GSYOSIN-AVG     PIC X(06).
      *    ���v�\���ϒʉ@��
             05  TA042-GTUUIN-AVG      PIC X(06).