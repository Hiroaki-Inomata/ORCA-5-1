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
      ************************************************************
      *    �ݸ�������ǯ�𡦳��׻��ѥ�᥿                      *
      *                         COPY  (CPORCSAGECHK.INC)          *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     NACL-¿��Ǽ  03/11/13  Ϸ�ͥե饰�ɲ�
      * 01.00.02     NACL-¿��Ǽ  04/04/21  Ϸ�Ͷ�ʬ�����ݸ���ʬ�ɲ�
      *                                     ��ǯ���ɲ�
      * 01.00.03     NACL-¿��Ǽ  04/05/13  ������ʬ�ɲ�
      * 01.00.04     NACL-¿��Ǽ  05/09/15  ����ñ�ȶ�ʬ�ɲ�
      * 03.00.01     NACL-¿��Ǽ  06/08/25  ʿ������ǯ����������б�
      * 03.05.00     NACL-¿��Ǽ  07/04/XX  ���롼�׿����б�
      * 04.02.00     NACL-¿��Ǽ  08/03/XX  ��������ʬ�ɲ�
      ************************************************************
       01  ORCSAGECHKAREA.
      *������
      *    ���ŵ��أɣġ�ɬ�ܡ�
           03  AGECHK-HOSPNUM          PIC 9(02).
      *    ���ԣɣġ�ɬ�ܡ�
           03  AGECHK-PTID             PIC 9(10).
      *    ��������ɬ�ܡ�
           03  AGECHK-SRYYMD           PIC X(08).
      *    ������ǯ����
           03  AGECHK-BIRTHDAY         PIC X(08).
      *    �ݸ��ȹ��ֹ�
           03  AGECHK-HKNCOMBINUM      PIC X(04).
      *    ������ʬ�ʣ����������ʳ��ϳ����
           03  AGECHK-NYUGAIKBN        PIC X(01).
      *������
           03  AGECHK-OUT-AREA.
      *        ǯ����ݸ���
               05  AGECHK-NENREI         PIC 9(03).
      *        �ʲ����ݸ��ȹ��ֹ����ϻ�
      *        ��ô���ʣ���̤����70�аʾ�Σ���������դΤߡ�
               05  AGECHK-FTNRATE        PIC 9(03).
      *        ��ô���̾��
               05  AGECHK-FTNRATEMEI     PIC X(10).
      *        Ϸ�ͥ��顼�ʣ���Ϸ���ݸ��ʤ������������аʾ�Ϸ�͸���ʤ�
      *                    ���������
               05  AGECHK-RJNCHK         PIC 9(01).
      *        �ݸ����顼�ʣ����ݸ��ʤ���
               05  AGECHK-HKNCHK         PIC 9(01).
      *        Ϸ�Ͷ�ʬ�ʣ������̡�����Ϸ�͡�
               05  AGECHK-ROUJIN         PIC 9(01).
      *        Ϸ�Ͷ�ʬ���ʣ������̡�����Ϸ�͡�
      *        ��������Ϸ�͸��񤬤������
               05  AGECHK-ROUJIN2        PIC 9(01).
      *        �ݸ���ʬ�ʣ���ϫ�ҡ����������ա��������񡢣����ʳ�
      *                            ���������աʷ��ݽ��ˡ�
      *                            ������̳�ҳ��ʷ��ݽ��ˡ�
               05  AGECHK-HKNKBN         PIC 9(01).
      *        ��ǯ��ʿ�������ǯ���
               05  AGECHK-NENREI2.
                   07  AGECHK-NENREI2-YY     PIC 9(03).
                   07  AGECHK-NENREI2-MM     PIC 9(02).
                   07  AGECHK-NENREI2-DD     PIC 9(02).
      *        ��������ñ�ȡʣ���ñ�ȡ�
               05  AGECHK-KOHCHK         PIC 9(01).
      *        �������������Ƚ��̵ͭ
               05  AGECHK-KAKUNINCHK     PIC 9(01).
      *        ��������ʬ�ʣ�����������
               05  AGECHK-SYUGAKUKBN     PIC 9(01).
      *    ����������
           03  AGECHK-RC              PIC 9(02).
      *
      *