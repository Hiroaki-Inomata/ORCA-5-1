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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCHC03V02.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ������Ͽ
      *  ����ݡ��ͥ��̾  : �����������μ��񡡰����ʣȣãͣ����֣�����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  04/01/29    NACL�ݾ�Ʀ��  ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-��Ʀ��  04/01/29  �ݸ�Ŭ�ѳ��ε��ܤȾ������б�
      *  01.00.02    NACL-����    04/03/17  �ʺơˤ�ʸ�����Խ�����
      *  01.00.03    NACL-¿��Ǽ  04/07/15  ʣ���ʡ��ݸ��ȹ礻�б�
      *  01.00.04    NACL-¿��Ǽ  05/01/11  ����̤���ۤ�ʣ���ʡ��ݸ��б�
      *  01.00.05    NACL-��Ʀ��  05/05/25  ϫ�ҡ������դδ�������ʬ�����٤�
      *                                     ɽ������褦���ѹ�
      *  01.00.06    NACL-¿��Ǽ  06/05/15  MONFUNC �б�
      *  01.00.07    NACL-����    06/08/22  �ѥ�᥿�ơ��֥����Խ��ǡ���
      *                                     �򻲾Ȥ����ʬ���ɲ�
      *                                     ��ޤȤ������ȯ�Ե�ǽ�ɲ�
      *  03.03.00    NACL-¿��Ǽ  06/10/02  �ֶ��б��ɲ�
      *  03.04.01    NACL-����    07/01/17  ���ŵ���̾�Ρ����ꥻ�å��ѹ�
      *  03.05.00    NACL-¿��Ǽ  07/04/XX  ���롼�׿����б�
      *  03.05.00    NACL-¿��Ǽ  07/05/22  �ư��������ɲ�
      *  03.05.00    NACL-����    07/08/03  ����ȯ�Լ����������ɲ�
      *  03.05.01    NACL-¿��Ǽ  07/08/06  ����������б�
      *  04.01.00    NACL-����    07/10/22  ��̳�ҳ��б�
      *  04.02.00    NACL-����    08/03/06  �����б�
      *  04.03.00    NACL-����    08/09/17  �����б�
      *  04.04.00    NACL-����    09/01/14  �ץ�����४�ץ�����б�
      *  04.05.00    NACL-����    09/07/24  ORCHCN03V03������
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    �����ѥ�᥿
           COPY    "CPORCHC03.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCHC03AREA
           .
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    "V2"            TO      ORCHC03-SIZE
           CALL    "ORCHC03V03"    USING   SPA-AREA
                                           ORCHC03AREA
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *