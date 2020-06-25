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
      *----------------------------------------------------------------*
      *    ���������ݸ��ֹ���Υ����ɥơ��֥� COPY (CMSANTEITBL.INC)
      *----------------------------------------------------------------*
      *****************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-¿��Ǽ  02/08/09  ���ܤε��������谷���ɲ�
      *  01.00.02    NACL-¿��Ǽ  04/03/15  �ȣ���ǯ��������ɲ�
      *                                     ��ã�����ɲ�
      *                                     ��������ˡ���ɲ�
      *  01.00.03    NACL-¿��Ǽ  04/12/02  ����ȯ�����ɲ�
      *  03.03.00    NACL-¿��Ǽ  06/10/20  �����������ɲ�
      *  03.04.00    NACL-¿��Ǽ  07/01/11  ��ϥӥ꽪λ���ɲ�
      *  03.04.01    NACL-¿��Ǽ  07/03/XX  ��ش������ɲ�
      *  04.08.00    NACL-¿��Ǽ  16/03/XX  �ȣ���ǯ��������б�
      *****************************************************************
      *
      *    ���������ݸ��ֹ���Υ�����
       01  TBL-SANTEI-AREA.
      *    ������ˡ�ʸ��̡ˣ���ñ�̥����å��ơ��֥�
           03  FILLER              PIC X(13)   VALUE   "KR 180022110".
           03  FILLER              PIC X(13)   VALUE   "KR 180022310".
           03  FILLER              PIC X(13)   VALUE   "KR 180022510".
           03  FILLER              PIC X(13)   VALUE   "KR 180022710".
      *    �����ˡ�ʸ��̡�
           03  FILLER              PIC X(13)   VALUE   "KS 180023610".
           03  FILLER              PIC X(13)   VALUE   "KS 180023810".
      *    ������ˡ�ʸ��̡�
           03  FILLER              PIC X(13)   VALUE   "KG 180024810".
           03  FILLER              PIC X(13)   VALUE   "KG 180025010".
      *    ������ˡ���ʸ��̡�
           03  FILLER              PIC X(13)   VALUE   "KG 180025410".
      *    ������ˡ�ʸ��̡ˡ�Ϸ�͡�
           03  FILLER              PIC X(13)   VALUE   "KR 180706810".
           03  FILLER              PIC X(13)   VALUE   "KR 180707010".
           03  FILLER              PIC X(13)   VALUE   "KR 180707210".
           03  FILLER              PIC X(13)   VALUE   "KR 180707410".
      *    �����ˡ�ʸ��̡ˡ�Ϸ�͡�
           03  FILLER              PIC X(13)   VALUE   "KS 180708110".
           03  FILLER              PIC X(13)   VALUE   "KS 180708310".
      *
      *    ������ˡ�ʽ��ġ�
           03  FILLER              PIC X(13)   VALUE   "SR 180022210".
           03  FILLER              PIC X(13)   VALUE   "SR 180022410".
           03  FILLER              PIC X(13)   VALUE   "SR 180022610".
           03  FILLER              PIC X(13)   VALUE   "SR 180022810".
      *    �����ˡ�ʽ��ġ�
           03  FILLER              PIC X(13)   VALUE   "SS 180023710".
           03  FILLER              PIC X(13)   VALUE   "SS 180023910".
      *    ������ˡ�ʽ��ġ�
           03  FILLER              PIC X(13)   VALUE   "SG 180024910".
           03  FILLER              PIC X(13)   VALUE   "SG 180025110".
      *    ������ˡ���ʽ��ġ�
           03  FILLER              PIC X(13)   VALUE   "SG 180025510".
      *    ������ˡ�ʽ��ġˡ�Ϸ�͡�
           03  FILLER              PIC X(13)   VALUE   "SR 180706910".
           03  FILLER              PIC X(13)   VALUE   "SR 180707110".
           03  FILLER              PIC X(13)   VALUE   "SR 180707310".
           03  FILLER              PIC X(13)   VALUE   "SR 180707510".
      *    �����ˡ�ʽ��ġˡ�Ϸ�͡�
           03  FILLER              PIC X(13)   VALUE   "SS 180708210".
           03  FILLER              PIC X(13)   VALUE   "SS 180708410".
      *
      *    �ñ����˽���
      *    �ޥå��������μ굻�ˤ��
           03  FILLER              PIC X(13)   VALUE   " M 140029610".
      *    ������ˤ��
           03  FILLER              PIC X(13)   VALUE   " K 140040310".
      *    ���۽��֣�
           03  FILLER              PIC X(13)   VALUE   " P 140002210".
      *    ���۽��֣�
           03  FILLER              PIC X(13)   VALUE   " P 140002310".
      *    ��ã����
           03  FILLER              PIC X(13)   VALUE   " T 140048010".
      *H18/04����
      *    ��ã�����ʶ��������
           03  FILLER              PIC X(13)   VALUE   "HT 140030350".
      *    ��ã�������ѷ����������ѡ�
           03  FILLER              PIC X(13)   VALUE   "HT 140048250".
      *    ���������Ӹ���
           03  FILLER              PIC X(13)   VALUE   "HE 140048350".
      *    ���������Ӹ���
           03  FILLER              PIC X(13)   VALUE   "HE 140048450".
      *    ����ϥ졼���ȼ�
           03  FILLER              PIC X(13)   VALUE   "HE 140048550".
      *    �������
           03  FILLER              PIC X(13)   VALUE   "HE 140002450".
      *
      *    ϫ�Ҥεߵް��Ŵ����û�
           03  FILLER              PIC X(13)   VALUE   "  R101110020".
           03  FILLER              PIC X(13)   VALUE   "  R101110030".
      *    ϫ�Ҥκƽ�ϫ���ܻ�Ƴ������
           03  FILLER              PIC X(13)   VALUE   "   101130010".
      *    ϫ�Ҥ����ܤε��������谷��
           03  FILLER              PIC X(13)   VALUE   "  R101130020".
      *    ϫ�Ҥν�ǻ��֥�å�����
           03  FILLER              PIC X(13)   VALUE   "  R101400030".
      *    ϫ�Ҥμ�ؤ��Ͻ��˷��뵡ǽ������Ƴ�û�
           03  FILLER              PIC X(13)   VALUE   "  R101500080".
      *
      *    Ǿ��ɵ���ȯ����
           03  FILLER              PIC X(13)   VALUE   "   099800101".
      *    Ǿ��ɵ���ȯ����λ
           03  FILLER              PIC X(13)   VALUE   "   099800102".
      *�ȣ�����������
      *    ��ϥӥ껻�����ʡ�����
           03  FILLER              PIC X(13)   VALUE   "   099800201".
      *    ��ϥӥ��ȯ����
           03  FILLER              PIC X(13)   VALUE   "  H0998001111".
           03  FILLER              PIC X(13)   VALUE   "  H0998001212".
           03  FILLER              PIC X(13)   VALUE   "  H0998001313".
           03  FILLER              PIC X(13)   VALUE   "  H0998001414".
           03  FILLER              PIC X(13)   VALUE   "   099800151".
           03  FILLER              PIC X(13)   VALUE   "   099800161".
           03  FILLER              PIC X(13)   VALUE   "   099800171".
           03  FILLER              PIC X(13)   VALUE   "  H0998001919".
      *    ��ϥӥ�ƽ�λ��
           03  FILLER              PIC X(13)   VALUE   "  E0998001121".
           03  FILLER              PIC X(13)   VALUE   "  E0998001222".
           03  FILLER              PIC X(13)   VALUE   "  E0998001323".
           03  FILLER              PIC X(13)   VALUE   "  E0998001424".
           03  FILLER              PIC X(13)   VALUE   "   099800152".
           03  FILLER              PIC X(13)   VALUE   "   099800162".
           03  FILLER              PIC X(13)   VALUE   "   099800172".
           03  FILLER              PIC X(13)   VALUE   "  E0998001929".
      *�����շ��ݽ���ɲáʣȣ������������
      *    �ãԡ��ͣңɻ���
           03  FILLER              PIC X(13)   VALUE   "   099700101".
      *��ϥӥ��ش������ɲáʣȣ������������
           03  FILLER              PIC X(13)   VALUE   "  S1800299101".
           03  FILLER              PIC X(13)   VALUE   "  S1800300101".
           03  FILLER              PIC X(13)   VALUE   "  S1800301102".
           03  FILLER              PIC X(13)   VALUE   "  S1800302102".
           03  FILLER              PIC X(13)   VALUE   "  S1800303103".
           03  FILLER              PIC X(13)   VALUE   "  S1800304103".
           03  FILLER              PIC X(13)   VALUE   "  S1800305104".
           03  FILLER              PIC X(13)   VALUE   "  S1800306104".
      *��ϥӥ��ش�����������μ»����ʣȣ���������
           03  FILLER              PIC X(13)   VALUE   "   099800311".
           03  FILLER              PIC X(13)   VALUE   "   099800321".
           03  FILLER              PIC X(13)   VALUE   "   099800331".
           03  FILLER              PIC X(13)   VALUE   "   099800341".
      *
       01  TBL-SANTEI-AREA-R       REDEFINES   TBL-SANTEI-AREA.
      **** 03  TBL-SANTEI-OCC      OCCURS   39  INDEXED   TBL-SAN.
      *****03  TBL-SANTEI-OCC      OCCURS   41  INDEXED   TBL-SAN.
      **** 03  TBL-SANTEI-OCC      OCCURS   65  INDEXED   TBL-SAN.
      **** 03  TBL-SANTEI-OCC      OCCURS   73  INDEXED   TBL-SAN.
           03  TBL-SANTEI-OCC      OCCURS   79  INDEXED   TBL-SAN.
      *        ��ϥӥ��K:���̡�S:���ġ�(H:�ȣ������������
               05  TBL-RIGAKU-RYO      PIC X(01).
      *        ��ϥӥ��R:����,S:���, G:����)
      *        �ñ����˽���(M:�ޥå�������,K:�������P:����
      *                     T:��ã������E:����¾)
               05  TBL-RIGAKU-KBN      PIC X(01).
      *        (R:ϫ���裱��),(S:��ϥӥ��ش�����)
               05  TBL-RSI-KBN         PIC X(01).
      *
               05  TBL-SANTEI-SRYCD    PIC X(09).
      *        �����̥�ϥӥ��ʬ(1:������,2:Ǿ���,3:��ư��,4:�Ƶ۴�
      *                           9:���Ѿɸ���)
               05  TBL-RIHA-SYUKBN     PIC X(01).
      *
      *    �ݸ��軻�ꤢ��
       01  FLG-HKNSAN                  PIC 9(01).
      *    ��ϥӥ�̵ͭ
       01  FLG-HKN-SANTEI              PIC 9(01).
      *    ϫ���軻��
       01  WRK-RSI-KBN                 PIC X(01).
      *