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
      *  �R���|�[�l���g��  : �������ꗗ�\
      *  �Ǘ���            : 
      *  Maj/Min/Rev  �C����  ���t       ���e
      *   01.00.00    ���    11/03/29   �V�K�쐬
      ******************************************************************
       01  TA03.
      *    �f�ÔN��������
           02  TA03-SRYSTYMD         PIC X(22).
      *    �f�ÔN��������
           02  TA03-SRYEDYMD         PIC X(22).
      *    �쐬��
           02  TA03-CREATEYMD        PIC X(22).
      *    �߰�ސ�
           02  TA03-PAGE             PIC X(2).
      *    ���߰�ސ�
           02  TA03-GPAGE            PIC X(2).
      *    ���
           02  TA03-NYUGAI           PIC X(6).
      *    �a���^�f�É�
           02  TA03-LABEL            PIC X(10).
           02  TA03-MEISAI-TBL       OCCURS  34 TIMES.
      *      �A��
             03  TA03-RENNUM         PIC X(6).
      *      ���Ҕԍ�
             03  TA03-PTNUM          PIC X(10).
      *      ���Ҕԍ�2-1
             03  TA03-PTNUM2-1       PIC X(10).
      *      ���Ҕԍ�2-2
             03  TA03-PTNUM2-2       PIC X(10).
      *      ����
             03  TA03-NAME           PIC X(20).
      *      �ی�
             03  TA03-HKN            PIC X(4).
      *      ��������
             03  TA03-SEIKYUMD       PIC X(9).
      *      �`�[�ԍ�
             03  TA03-DENPNUM        PIC X(7).
      *      �a���^�f�É�
             03  TA03-SRYKA-BR       PIC X(8).
      *      �ی��v
             03  TA03-HKN-TOTAL      PIC X(9).
      *      ���ҕ��S
             03  TA03-KANJAFTN       PIC X(9).
      *      ����
             03  TA03-JIHI           PIC X(9).
      *      �����z
             03  TA03-MISYU          PIC X(9).
      *      �����z
             03  TA03-NYUKIN         PIC X(9).
      *      ��������
             03  TA03-NYUKINMD       PIC X(9).
      *      �ŏI��f��
             03  TA03-JYUSHINYMD     PIC X(9).
      *      �d�b�ԍ�
             03  TA03-TEL            PIC X(15).
      *    ���v
           02  TA03-GOKEI-TBL.
      *      ����
             03  TA03-GKENSU         PIC X(6).
      *      �ی��v
             03  TA03-GHKN-TOTAL     PIC X(10).
      *      ���ҕ��S
             03  TA03-GKANJAFTN      PIC X(10).
      *      ����
             03  TA03-GJIHI          PIC X(10).
      *      �����z
             03  TA03-GMISYU         PIC X(10).
      *      �����z
             03  TA03-GNYUKIN        PIC X(10).