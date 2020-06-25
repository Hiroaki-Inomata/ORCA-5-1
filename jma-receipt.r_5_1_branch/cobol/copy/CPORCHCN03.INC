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
      *    �������μ�����ϥѥ�᥿      COPY (CPORCHCN03.INC)
      *                                          (ORCHCN03 �ǻ��ѡ�
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    MCC��¿��Ǽ  02/03/14  ��������ۤ��ɲ�
      *  02.07.00    NACL-����    06/02/09  ������ʬ���ɲ�
      *  04.00.00    NACL-����    07/10/18  �ѻ極�������ɲ�
      ************************************************************
      *
       01  ORCHCN03AREA.
           03  ORCHCN03-HEAD-HOSPNUM       PIC 9(2).
           03  ORCHCN03-STAFFCD            PIC X(05).
           03  ORCHCN03-SYSYMD             PIC X(08).
           03  ORCHCN03-TERMID             PIC X(64).
      *    ����������ˡ�ե饰
      *    0:��Ǽ�ǡ�����ȯ����ˡ�˽���
      *    1:���̤�ȯ��
           03  ORCHCN03-SKYPRTFLG          PIC 9(01).
           03  ORCHCN03-CNT                PIC 9(10).
           03  ORCHCN03-OCC                OCCURS  3000.
      *        �����
               05  ORCHCN03-KJNYMD         PIC  X(08).
      *        ���ŵ��أɣ�
               05  ORCHCN03-HOSPNUM        PIC 9(2).
      *        ���ԣɣ�
               05  ORCHCN03-PTID           PIC  9(10).
      *        �����ֹ�
               05  ORCHCN03-PTNUM          PIC  X(20).
      *        ������ʬ
               05  ORCHCN03-NYUGAIKBN      PIC X(01).
      *        ��ɼ�ֹ�
               05  ORCHCN03-DENPNUM        PIC 9(07).
      *        ��������ݾ����ǡʺƷǡ�
               05  ORCHCN03-SEIKYU-TAX-SAI PIC S9(07).
      *        ���������
               05  ORCHCN03-SEIKYU         PIC S9(07).
      *        ���������
               05  ORCHCN03-NYUKIN         PIC S9(07).
      *        ȯ�ԥե饰�ʣ���������������ȯ�ԡ���������
      *                    ������Ǽ�����ǧ���̤����
               05  ORCHCN03-HAKKOFLG       PIC 9(01).
      *        ��ɼ�ֹ����
               05  ORCHCN03-DENPEDANUM     PIC 9(02).
      *        ��������̤����
               05  ORCHCN03-KONMISYU       PIC S9(07).
      *        ����ǯ����
               05  ORCHCN03-SRYYMD         PIC X(08).
      *        �ݸ��ȹ礻
               05  ORCHCN03-HKNCOMBI       PIC X(04).
      *
           03  ORCHCN03-PRTKBN             PIC X(1).
           03  ORCHCN03-PRTAREA            PIC X(25000).
           03  ORCHCN03-OUTCNT             PIC 9(10).
           03  ORCHCN03-SIZE               PIC X(02).
      *
      *    ���긵��̳
      *    0:���ౡ��Ͽ��̳
      *    1:��Ǽ��̳�ʺ�ȯ�ԡ�
      *    2:��Ǽ��̳�ʷ��������ˡ�����Τ�
      *    3:��Ǽ��̳�������ǧ���̡�
      *    4:��Ǽ��̳�ʰ��������̡�
      *    5:����׶�̳�ʷ��������ˡ�����Τ�
      *    6:��������̳�������Τ�
      *    7:��������̳(�����ǧ����)�������Τ�
      *    8:�ౡ�����׻��������Τ�
           03  ORCHCN03-MOTOID           PIC 9(02).
      *    ������ȯ�ԥե饰
           03  ORCHCN03-GAISAN-FLG       PIC 9(01).