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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *---------------------------------------------------*
      *    ���������ѹ��ѥ�᥿                          *
      *                         COPY  (CPORCSSANTEI.INC) *
      *
      *---------------------------------------------------*
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-¿��Ǽ  02/12/05  ������������ɲ�
      *  01.00.02    NACL-¿��Ǽ  05/03/03  ����������������ʬ�ɲ�
      *  01.00.03    NACL-¿��Ǽ  06/03/15  �����ɲ�
      *  03.04.00    NACL-¿��Ǽ  06/12/01  �����շ��ݽ���ɲ�
      *  04.01.00    NACL-¿��Ǽ  07/10/19  ��̳�ҳ��ɲ�
      *  04.07.00    NACL-¿��Ǽ  11/08/XX  ���������ղú���ɲ�
      *****************************************************************
       01  ORCSSANTEIAREA.
      *
      *������ʬ�ʣ����ɲá����������������
           03  SSANTEI-SYORI             PIC  X(01).
      *������ʬ���ʣ������������ղú����
           03  SSANTEI-SYORI2            PIC  X(01).
      *�����У�
           03  SSANTEI-PG                PIC  X(03).
      *�оݥ�����
           03  SSANTEI-SRYCD             PIC  X(09).
      *����ǯ����
           03  SSANTEI-SRYYMD            PIC  X(08).
      *���ǯ������ɬ�פʻ��Τߡ�
           03  SSANTEI-SYOKAIYMD         PIC  X(08).
      *���ԣɣ�
           03  SSANTEI-PTID              PIC  9(10).
      *������
           03  SSANTEI-KAISU             PIC  9(08).
      *������̡ʥ�ϥӥ�¾��
           03  SSANTEI-SRYSURYO          PIC  9(05).
      *������������ʿ����ǣˣ����λ��Τߡ�
           03  SSANTEI-SYOYMD            PIC  X(08).
      *���Ų�
           03  SSANTEI-SRYKA             PIC  X(02).
      *�ݸ��ȹ礻
           03  SSANTEI-HKNCOMBINUM       PIC  X(04).
      *�ݸ���ʬ
           03  SSANTEI-HKNKBN            PIC  X(01).
      *ϫ�ҡ������ն�ʬ
           03  SSANTEI-RSI-HKNKBN        PIC  X(01).
      *ϫ�ҡ������� ���ݽ��
           03  SSANTEI-RSI-JUNKYO        PIC  X(01).
      *��ʬ����ٻ��
      *���������
           03  SSANTEI-MSANTEITEN        PIC  9(08).
      *�����������ʬ
           03  SSANTEI-MSANTEITENKBN     PIC  9(01).
      *
      *    ��繹���λ��ʣʣ�����
           03  SSANTEI-IKKATU.
      *������ʬ
               05  SSANTEI-NYUGAIKBN       PIC  X(01).
      *����������
               05  SSANTEI-MSANTEID        PIC  X(02).
      *�������
               05  SSANTEI-MSANTEICNT      PIC  9(03).
      *�ѹ����Ų���ơ��֥�
               05  SSANTEI-DAY-AREA.
                   07  SSANTEI-DAY         PIC  9(03)
                                           OCCURS   31.
      *�ѹ������Ų���ơ��֥�
               05  SSANTEI-MAE-DAY-AREA.
                   07  SSANTEI-MAE-DAY     PIC  9(03)
                                           OCCURS   31.
      *
      *��ʬ����ٻ����ե饰
           03  SSANTEI-MSANTEIDEL        PIC  9(01).
      *���������ղþ��󤢤�ʺ����
           03  SSANTEI-SANTEIPLUS        PIC  9(01).
      *
