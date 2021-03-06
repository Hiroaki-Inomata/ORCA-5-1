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
      *-------------------------------------------------*
      *    ñ�̥����ɥơ��֥�   COPY (CMTANICD.INC)     *
      *-------------------------------------------------*
      *****************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     MCC-¿��Ǽ   02/03/19  124 �ݼ��ɲ�(H14.4����)
      * 01.00.02     NACL-¿��Ǽ  02/12/13  060 �������ɲ�(H14.12����)
      * 01.00.03     NACL-¿��Ǽ  04/12/22  000 ñ�̤ʤ��ɲ�
      * 01.00.04     NACL-¿��Ǽ  06/03/15  125 ���� 128�ɲ�(H18.4������
      * 04.08.00     NACL-¿��Ǽ  16/08/30  061 �����å��ɲ�
      *****************************************************************
       01  MTANI-DATA-REC.
           03  FILLER          PIC X(03)  VALUE   "000".
           03  FILLER          PIC X(24)  VALUE   SPACE.
      *
           03  FILLER          PIC X(03)  VALUE   "001".
           03  FILLER          PIC X(24)  VALUE   "ʬ".
      *
           03  FILLER          PIC X(03)  VALUE   "002".
           03  FILLER          PIC X(24)  VALUE   "��".
      *
           03  FILLER          PIC X(03)  VALUE   "003".
           03  FILLER          PIC X(24)  VALUE   "��".
      *
           03  FILLER          PIC X(03)  VALUE   "004".
           03  FILLER          PIC X(24)  VALUE   "Ȣ".
      *
           03  FILLER          PIC X(03)  VALUE   "005".
           03  FILLER          PIC X(24)  VALUE   "��".
      *
           03  FILLER          PIC X(03)  VALUE   "006".
           03  FILLER          PIC X(24)  VALUE   "��".
      *
           03  FILLER          PIC X(03)  VALUE   "007".
           03  FILLER          PIC X(24)  VALUE   "��".
      *
           03  FILLER          PIC X(03)  VALUE   "008".
           03  FILLER          PIC X(24)  VALUE   "��".
      *
           03  FILLER          PIC X(03)  VALUE   "009".
           03  FILLER          PIC X(24)  VALUE   "���å�".
      *
           03  FILLER          PIC X(03)  VALUE   "010".
           03  FILLER          PIC X(24)  VALUE   "��".
      *
           03  FILLER          PIC X(03)  VALUE   "011".
           03  FILLER          PIC X(24)  VALUE   "��".
      *
           03  FILLER          PIC X(03)  VALUE   "012".
           03  FILLER          PIC X(24)  VALUE   "����".
      *
           03  FILLER          PIC X(03)  VALUE   "013".
           03  FILLER          PIC X(24)  VALUE   "�ȥ�����".
      *
           03  FILLER          PIC X(03)  VALUE   "014".
           03  FILLER          PIC X(24)  VALUE   "����ץ�".
      *
           03  FILLER          PIC X(03)  VALUE   "015".
           03  FILLER          PIC X(24)  VALUE   "���ץ���".
      *
           03  FILLER          PIC X(03)  VALUE   "016".
           03  FILLER          PIC X(24)  VALUE   "��".
      *
           03  FILLER          PIC X(03)  VALUE   "017".
           03  FILLER          PIC X(24)  VALUE   "��".
      *
           03  FILLER          PIC X(03)  VALUE   "018".
           03  FILLER          PIC X(24)  VALUE   "��".
      *
           03  FILLER          PIC X(03)  VALUE   "019".
           03  FILLER          PIC X(24)  VALUE   "��".
      *
           03  FILLER          PIC X(03)  VALUE   "020".
           03  FILLER          PIC X(24)  VALUE   "��".
      *
           03  FILLER          PIC X(03)  VALUE   "021".
           03  FILLER          PIC X(24)  VALUE   "�ӡ��ޡ�".
      *
           03  FILLER          PIC X(03)  VALUE   "022".
           03  FILLER          PIC X(24)  VALUE   "��".
      *
           03  FILLER          PIC X(03)  VALUE   "023".
           03  FILLER          PIC X(24)  VALUE   "�����".
      *
           03  FILLER          PIC X(03)  VALUE   "024".
           03  FILLER          PIC X(24)  VALUE   "��ʬ".
      *
           03  FILLER          PIC X(03)  VALUE   "025".
           03  FILLER          PIC X(24)  VALUE   "�ƥ���ʬ".
      *
           03  FILLER          PIC X(03)  VALUE   "026".
           03  FILLER          PIC X(24)  VALUE   "���饹��".
      *
           03  FILLER          PIC X(03)  VALUE   "027".
           03  FILLER          PIC X(24)  VALUE   "���".
      *
           03  FILLER          PIC X(03)  VALUE   "028".
           03  FILLER          PIC X(24)  VALUE   "ñ��".
      *
           03  FILLER          PIC X(03)  VALUE   "029".
           03  FILLER          PIC X(24)  VALUE   "��ñ��".
      *
           03  FILLER          PIC X(03)  VALUE   "030".
           03  FILLER          PIC X(24)  VALUE   "�ե�����".
      *
           03  FILLER          PIC X(03)  VALUE   "031".
           03  FILLER          PIC X(24)  VALUE   "ũ".
      *
           03  FILLER          PIC X(03)  VALUE   "032".
           03  FILLER          PIC X(24)  VALUE   "����".
      *
           03  FILLER          PIC X(03)  VALUE   "033".
           03  FILLER          PIC X(24)  VALUE   "��".
      *
           03  FILLER          PIC X(03)  VALUE   "034".
           03  FILLER          PIC X(24)  VALUE   "�ˣ�".
      *
           03  FILLER          PIC X(03)  VALUE   "035".
           03  FILLER          PIC X(24)  VALUE   "���".
      *
           03  FILLER          PIC X(03)  VALUE   "036".
           03  FILLER          PIC X(24)  VALUE   "����".
      *
           03  FILLER          PIC X(03)  VALUE   "037".
           03  FILLER          PIC X(24)  VALUE   "��".
      *
           03  FILLER          PIC X(03)  VALUE   "038".
           03  FILLER          PIC X(24)  VALUE   "���̣�".
      *
           03  FILLER          PIC X(03)  VALUE   "039".
           03  FILLER          PIC X(24)  VALUE   "�Х�����".
      *
           03  FILLER          PIC X(03)  VALUE   "040".
           03  FILLER          PIC X(24)  VALUE   "���".
      *
           03  FILLER          PIC X(03)  VALUE   "041".
           03  FILLER          PIC X(24)  VALUE   "�����".
      *
           03  FILLER          PIC X(03)  VALUE   "042".
           03  FILLER          PIC X(24)  VALUE   "��".
      *
           03  FILLER          PIC X(03)  VALUE   "043".
           03  FILLER          PIC X(24)  VALUE   "�̣ã�".
      *
           03  FILLER          PIC X(03)  VALUE   "044".
           03  FILLER          PIC X(24)  VALUE   "���ã�".
      *
           03  FILLER          PIC X(03)  VALUE   "045".
           03  FILLER          PIC X(24)  VALUE   "�̣�".
      *
           03  FILLER          PIC X(03)  VALUE   "046".
           03  FILLER          PIC X(24)  VALUE   "�ɡ��ӡ�".
      *
           03  FILLER          PIC X(03)  VALUE   "047".
           03  FILLER          PIC X(24)  VALUE   "��".
      *
           03  FILLER          PIC X(03)  VALUE   "048".
           03  FILLER          PIC X(24)  VALUE   "�ǣ£�".
      *
           03  FILLER          PIC X(03)  VALUE   "049".
           03  FILLER          PIC X(24)  VALUE   "�ͣ£�".
      *
           03  FILLER          PIC X(03)  VALUE   "050".
           03  FILLER          PIC X(24)  VALUE   "�ˣ£�".
      *
           03  FILLER          PIC X(03)  VALUE   "051".
           03  FILLER          PIC X(24)  VALUE   "���å�".
      *
           03  FILLER          PIC X(03)  VALUE   "052".
           03  FILLER          PIC X(24)  VALUE   "���ñ��".
      *
           03  FILLER          PIC X(03)  VALUE   "053".
           03  FILLER          PIC X(24)  VALUE   "��������".
      *
           03  FILLER          PIC X(03)  VALUE   "054".
           03  FILLER          PIC X(24)  VALUE   "����".
      *
           03  FILLER          PIC X(03)  VALUE   "055".
           03  FILLER          PIC X(24)  VALUE   "��".
      *
           03  FILLER          PIC X(03)  VALUE   "056".
           03  FILLER          PIC X(24)  VALUE   "�������".
      *
           03  FILLER          PIC X(03)  VALUE   "057".
           03  FILLER          PIC X(24)  VALUE   "�ƴ�".
      *
           03  FILLER          PIC X(03)  VALUE   "058".
           03  FILLER          PIC X(24)  VALUE   "���̡ʣ��".
      *
           03  FILLER          PIC X(03)  VALUE   "059".
           03  FILLER          PIC X(24)  VALUE   "�֥ꥹ����".
      *
           03  FILLER          PIC X(03)  VALUE   "060".
           03  FILLER          PIC X(24)  VALUE   "������".
      *H28.8
           03  FILLER          PIC X(03)  VALUE   "061".
           03  FILLER          PIC X(24)  VALUE   "�����å�".
      *
      *
           03  FILLER          PIC X(03)  VALUE   "101".
           03  FILLER          PIC X(24)  VALUE   "ʬ��".
      *
           03  FILLER          PIC X(03)  VALUE   "102".
           03  FILLER          PIC X(24)  VALUE   "����".
      *
           03  FILLER          PIC X(03)  VALUE   "103".
           03  FILLER          PIC X(24)  VALUE   "����".
      *
           03  FILLER          PIC X(03)  VALUE   "104".
           03  FILLER          PIC X(24)  VALUE   "��".
      *
           03  FILLER          PIC X(03)  VALUE   "105".
           03  FILLER          PIC X(24)  VALUE   "�ݳ�".
      *
           03  FILLER          PIC X(03)  VALUE   "106".
           03  FILLER          PIC X(24)  VALUE   "�ȼ�".
      *
           03  FILLER          PIC X(03)  VALUE   "107".
           03  FILLER          PIC X(24)  VALUE   "¡��".
      *
           03  FILLER          PIC X(03)  VALUE   "108".
           03  FILLER          PIC X(24)  VALUE   "��".
      *
           03  FILLER          PIC X(03)  VALUE   "109".
           03  FILLER          PIC X(24)  VALUE   "����".
      *
           03  FILLER          PIC X(03)  VALUE   "110".
           03  FILLER          PIC X(24)  VALUE   "��".
      *
           03  FILLER          PIC X(03)  VALUE   "111".
           03  FILLER          PIC X(24)  VALUE   "�ɽ�".
      *
           03  FILLER          PIC X(03)  VALUE   "112".
           03  FILLER          PIC X(24)  VALUE   "����".
      *
           03  FILLER          PIC X(03)  VALUE   "113".
           03  FILLER          PIC X(24)  VALUE   "�������".
      *
           03  FILLER          PIC X(03)  VALUE   "114".
           03  FILLER          PIC X(24)  VALUE   "����".
      *
           03  FILLER          PIC X(03)  VALUE   "115".
           03  FILLER          PIC X(24)  VALUE   "����".
      *
           03  FILLER          PIC X(03)  VALUE   "116".
           03  FILLER          PIC X(24)  VALUE   "��".
      *
           03  FILLER          PIC X(03)  VALUE   "117".
           03  FILLER          PIC X(24)  VALUE   "��".
      *
           03  FILLER          PIC X(03)  VALUE   "118".
           03  FILLER          PIC X(24)  VALUE   "��".
      *
           03  FILLER          PIC X(03)  VALUE   "119".
           03  FILLER          PIC X(24)  VALUE   "¦".
      *
           03  FILLER          PIC X(03)  VALUE   "120".
           03  FILLER          PIC X(24)  VALUE   "�Ľ�".
      *
           03  FILLER          PIC X(03)  VALUE   "121".
           03  FILLER          PIC X(24)  VALUE   "��".
      *
           03  FILLER          PIC X(03)  VALUE   "122".
           03  FILLER          PIC X(24)  VALUE   "�Ǵ�".
      *
           03  FILLER          PIC X(03)  VALUE   "123".
           03  FILLER          PIC X(24)  VALUE   "��".
      *
           03  FILLER          PIC X(03)  VALUE   "124".
           03  FILLER          PIC X(24)  VALUE   "�ݼ�".
      *
           03  FILLER          PIC X(03)  VALUE   "125".
           03  FILLER          PIC X(24)  VALUE   "����".
      *
           03  FILLER          PIC X(03)  VALUE   "126".
           03  FILLER          PIC X(24)  VALUE   "�ս�".
      *
           03  FILLER          PIC X(03)  VALUE   "127".
           03  FILLER          PIC X(24)  VALUE   "�ǵ�".
      *
           03  FILLER          PIC X(03)  VALUE   "128".
           03  FILLER          PIC X(24)  VALUE   "��".
      *H30.4
           03  FILLER          PIC X(03)  VALUE   "131".
           03  FILLER          PIC X(24)  VALUE   "��".
      *
           03  FILLER          PIC X(03)  VALUE   "140".
           03  FILLER          PIC X(24)  VALUE   "����".
      *H24.4
           03  FILLER          PIC X(03)  VALUE   "158".
           03  FILLER          PIC X(24)  VALUE   "����".
      *
      *
       01  MTANI-DATA-REC-R         REDEFINES   MTANI-DATA-REC.
      *****03  MTANI-TBL-OCC        OCCURS    59   INDEXED   MTANI-IDX.
      **** 03  MTANI-TBL-OCC        OCCURS    83   INDEXED   MTANI-IDX.
           03  MTANI-TBL-OCC        OCCURS    93   INDEXED   MTANI-IDX.
               05  MTANI-TANICD        PIC X(03).
               05  MTANI-TANINAME      PIC X(24).
      *
       01  MTANI-MAX                   PIC 9(04)   VALUE   93.
      *
