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
      *----------------------------------------------------------*
      *    ���Ź԰����Ϲ����Ѵ��ѥ�᥿��  COPY (CPORCSC97.INC)
      *                              (ORCSC97 �ǻ��ѡ�
      *
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.06.00    NACL-¿��Ǽ  11/02/14  ��ˡ�������б�
      *  04.08.00    NACL-¿��Ǽ  13/12/20  �������б�
      ************************************************************
      *
       01  ORCSC97AREA.
      *    �У�̾
           03  LNK-SC97-PG             PIC X(06).
      *    ������ʬ�ʶ���������
      *              ����Ⱦ���Ѵ��Τߡ�
           03  LNK-SC97-KBN            PIC X(01).
      *    ���Ͻ����ΰ�
           03  LNK-SC97-INPUTCD        PIC X(54).
      *    �ӣУ��饤��
           03  LNK-SC97-GMN-IDX        PIC 9(04).
      *    ���̥饤��
           03  LNK-SC97-MAP-IDX        PIC 9(04).
      *    ���ֳ�������ŵ���
           03  LNK-SC97-JIKANTOKU          PIC  9(01).
      *
      *    �����ΰ�
           03  LNK-SC97-OUTAREA.
      *        �ǡ���̵ͭ
               05  LNK-SC97-JODOU          PIC 9(01).
      *        �������ѹ�̵ͭ�ʣ����ѹ��ʤ��������ѹ������
               05  LNK-SC97-NYURYOKU       PIC 9(01).
      *        ���ϥ�����
               05  LNK-SC97-INCD           PIC X(54).
      *        ���ϥ����ɷ��
               05  LNK-SC97-INCDCNT        PIC 9(02).
      *        ���ϥ����ɼ���
               05  LNK-SC97-CDSYU          PIC X(01).
      *        �Խ���
               05  LNK-SC97-INPUT-G.
      *        ����
                   07  LNK-SC97-SURYO          PIC 9(07)V9(05).
      *        ���
                   07  LNK-SC97-KAISU          PIC 9(03).
      *        ʬ���
                   07  LNK-SC97-BUNGSU         PIC 9(03).
      *        ����������
                   07  LNK-SC97-MOJI1          PIC X(10).
                   07  LNK-SC97-MOJI2          PIC X(10).
                   07  LNK-SC97-MOJI3          PIC X(10).
                   07  LNK-SC97-MOJI4          PIC X(10).
                   07  LNK-SC97-MOJI5          PIC X(10).
      *        ���ֳ��ե饰
                   07  LNK-SC97-JIKAN          PIC 9(01).
      *
      *        ����
                   07  LNK-SC97-SURYOFLG      PIC 9(02).
                   07  LNK-SC97-SURYOCNT      PIC 9(02).
      *        ����ʡ������
                   07  LNK-SC97-KAISUCNT       PIC 9(02).
      *        ����ʡ������
                   07  LNK-SC97-KAISUFLG       PIC 9(01).
      *        ʬ���
                   07  LNK-SC97-BUNGSUFLG      PIC 9(01).
      *        ����������
                   07  LNK-SC97-MCNT1          PIC 9(02).
                   07  LNK-SC97-MCNT2          PIC 9(02).
                   07  LNK-SC97-MCNT3          PIC 9(02).
                   07  LNK-SC97-MCNT4          PIC 9(02).
                   07  LNK-SC97-MCNT5          PIC 9(02).
      *        �����Ѵ�
                   07  LNK-SC97-AUTOFLG2        PIC X(01).
      *        ����������
                   07  LNK-SC97-INKBNFLG1       PIC X(01).
      *        ���������
                   07  LNK-SC97-INKBNFLG2       PIC X(01).
      *        ��³������
                   07  LNK-SC97-CONTFLG         PIC X(01).
      *!!!!!!!!!!!!!!!!!!
      *        �����Կ�
                   07  LNK-SC97-INSCNT          PIC 9(02).
      *        ����Ϣ³
                   07  LNK-SC97-INSREN          PIC 9(01).
      *!!!!!!!!!!!!!!!!!!
      *
      *        �������̤�
               05  LNK-SC97-KENSAKU        PIC X(01).
      *        �������
               05  LNK-SC97-NYUIN-KAISU.
                   07  LNK-SC97-NYU-DAY    PIC 9(02)
                                           OCCURS   31.
      *