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
      *  �R���|�[�l���g��  : �f�Ís�וʏW�v�\
      *  �Ǘ���            : 
      *  Maj/Min/Rev  �C����  ���t        ���e
      *   01.00.00    �c��    2011/02/14  �V�K�쐬
      *   01.00.01    ���    2014/05/22  �򉿁^�_���A�����A�g�p�ʂ������_��2�ʂ܂őΉ�
      *   01.00.02    ���    2017/06/30  �O�����z�A���@���z���̌�����ύX
      *******************************************************************
       01  TA02.
      *    �f�ÔN��
           03  TA02-PRTYMD              PIC X(50).
      *    �O���[�v�^�C�g��
           03  TA02-GRPTITLE            PIC X(10).
      *    �f�Ís�׋敪��
           03  TA02-SRYKBNNAME          PIC X(16).
      *    �쐬�N����
           03  TA02-CREATEYMD           PIC X(22).
      *    �y�[�W��
           03  TA02-PAGE                PIC X(02).
      *    ���v�y�[�W��
           03  TA02-GPAGE               PIC X(02).
      *    �Ȗ��i��t���j
           03  TA02-KANAME              PIC X(20).
      *    ���׃e�[�u��
           03  TA02-MEISAI-TBL          OCCURS 34 TIMES.
      *    �f��
               05  TA02-SRYKBN          PIC X(02).
      *    �R�[�h
               05  TA02-SRYCD           PIC X(09).
      *    �f�Ö���
               05  TA02-SRYNAME         PIC X(40).
      *    �򉿁^�_��
               05  TA02-YAKKA           PIC X(12).
      *    �O������
               05  TA02-GAIRAI-KEN      PIC X(09).
      *    ���@����
               05  TA02-NYUIN-KEN       PIC X(09).
      *    ���v����
               05  TA02-TOTAL-KEN       PIC X(10).
      *    �P��
               05  TA02-TANKA           PIC X(12).
      *    �w����
               05  TA02-KONYU           PIC X(09).
      *    �O���g�p��
               05  TA02-GAIRAI-SIYO     PIC X(09).
      *    ���@�g�p��
               05  TA02-NYUIN-SIYO      PIC X(09).
      *    ���v�g�p��
               05  TA02-TOTAL-SIYO      PIC X(10).
      *    �O�����z
               05  TA02-GAIRAI-KIN      PIC X(11).
      *    ���@���z
               05  TA02-NYUIN-KIN       PIC X(11).
      *    ���v���z
               05  TA02-TOTAL-KIN       PIC X(11).
      *    �w�����z
               05  TA02-KONYU-KIN       PIC X(09).
      *    ���z
               05  TA02-SAGAKU          PIC X(09).
      *    �S�̔䗦
               05  TA02-ZENHI           PIC X(05).
      *    �O���[�v�䗦
               05  TA02-GRPHI           PIC X(05).
      *    ���v�e�[�u��
           03  TA02-GOKEI-TBL.
      *    ���v�|�򉿁^�_��
               05  TA02-GYAKKA          PIC X(10).
      *    ���v�|�O������
               05  TA02-GGAIRAI-KEN     PIC X(10).
      *    ���v�|���@����
               05  TA02-GNYUIN-KEN      PIC X(10).
      *    ���v�|���v����
               05  TA02-GTOTAL-KEN      PIC X(11).
      *    ���v�|�P��
               05  TA02-GTANKA          PIC X(10).
      *    ���v�|�w����
               05  TA02-GKONYU          PIC X(10).
      *    ���v�|�O���g�p��
               05  TA02-GGAIRAI-SIYO    PIC X(10).
      *    ���v�|���@�g�p��
               05  TA02-GNYUIN-SIYO     PIC X(10).
      *    ���v�|���v�g�p��
               05  TA02-GTOTAL-SIYO     PIC X(11).
      *    ���v�|�O�����z
               05  TA02-GGAIRAI-KIN     PIC X(11).
      *    ���v�|���@���z
               05  TA02-GNYUIN-KIN      PIC X(11).
      *    ���v�|���v���z
               05  TA02-GTOTAL-KIN      PIC X(12).
      *    ���v�|�w�����z
               05  TA02-GKONYU-KIN      PIC X(10).
      *    ���v�|���z
               05  TA02-GSAGAKU         PIC X(10).
      *    ���v�|�S�̔䗦
               05  TA02-GZENHI          PIC X(05).
      *    ���v�|�O���[�v�䗦
               05  TA02-GGRPHI          PIC X(05).