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
      *    ��̾�����ɸ����ѥѥ�᥿      COPY (CPORCSBYOMEI.INC)
      *                                  ���ѥץ������ (ORCSBYOMEI)
      *    �������
      *      ������ʬ  ������̾  ������̾������
      *      ����̾�ե饰
      *      ��̾
      *      ������
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  00.00.01    NACL-ƣ��    04/03/17  �������������ե饰���ɲ�
      *
      *  04.05.01    NACL-ƣ��    09/11/25  �쥻�ť����ȳ�ĥ�б�
      *                                     ��­�����Ȥ��ɲ�
      *                                     ñ�Ȼ��Ѷػ���̾���ɲ�
      *
      *  04.06.01    NACL-ƣ��    10/10/20  ���³����Ƴ�������б�
      *                                     ���³��襳���ɤ��ɲ�
      *
      *  04.07.01    NACL-ƣ��    11/06/10  ��̾���饳���ɸ�������ʣ����
      *                                     �Ȥ�Ω�Ƥ��ֵѲ�ǽ�ˤ���
      ************************************************************
      *
       01  ORCSBYOMEIAREA.
      *    ������ʬ    ������̾����Խ�
      *                ������̾�����ɤ���Խ�
      *                ������̾����Խ�����­�����Ȥ����
      *                ������̾�����ɤ���Խ�����­�����Ȥ����
           03  LNK-BYO-SYORI           PIC X(01).
      *    ����������  ����������ʬ���顼
      *                ������̤̾����
      *                ������̾������̤����
      *                ��������̾�ե饰���顼
      *                ����������ʬ���ΤȤ���̾�԰��ץ��顼
           03  LNK-BYO-RC              PIC X(01).
      *    ����̾�ե饰
      *                ��������̾ 
           03  LNK-BYO-SYUBYOFLG       PIC X(01).
      *    ��̾
           03  LNK-BYO-BYOMEI          PIC X(200).
      *    ������
           03  LNK-BYO-SRYYMD-IN       PIC 9(08).
      *    �����ե饰
      *                ��������
      *                ��������
      *                �����������ĵ���
           03  LNK-BYO-UTAGAIFLG-IN    PIC X(01).
      *
      *    ������̾����
           03  LNK-BYOMEI-AREA.
      *    ��̾����
             05  LNK-BYOMEI-KENSAKU-AREA.
      *        ������̾������
               07  LNK-BYO-KHNBYOMEICD PIC X(07).
      *        �������̥�����
               07  LNK-BYO-KHNBUICD    PIC X(07)   OCCURS  3.
      *        ��̾�����ɿ�
               07  LNK-BYO-BYOMEICDSU  PIC 9(02).
      *        �����ե饰
               07  LNK-BYO-UTAGAIFLG   PIC X(01).
      *        �����ե饰
               07  LNK-BYO-MANSEIKBN   PIC 9(02).
      *        ���³��襳����
               07  LNK-BYO-NANBYOCD    PIC 9(02).
      *        ��̾ʸ����
               07  LNK-BYO-BYOMEIMOJI  PIC 9(03).
      *        ��̾�Խ��ե饰
               07  LNK-BYO-BYOMEIHENFLG
                                       PIC X(01).
      *        �ܹ��襳����
               07  LNK-BYO-IKOSAKICD   PIC X(07).
      *        �ܹ�����̾
               07  LNK-BYO-IKOSAKICD-BYOMEI
                                       PIC X(200).
      *        �ܹ�����̾������
               07  LNK-BYO-IKOSAKICD-INDEX
                                       PIC 9(02).
      *        ��̾���� 
               07  LNK-BYO-BYOMEISBT   PIC 9(01). 
      *        �쥻�ť��顼�ե饰
               07  LNK-BYO-RECEDENFLG  PIC X(01).
      *
      *    ������̾���顼����
             05  LNK-BYOMEI-KENSAKU-ERR-AREA.
      *        ����Ⱦ�ѥ����å����顼������������ʾ�
               07  LNK-BYO-KANACHK-ERR PIC 9(01).
      *        ñ�Ȼ��Ѷػߥ��顼
               07  LNK-BYO-TANDOKUKBN-ERR
                                       PIC 9(01).
      *        ñ�Ȼ��Ѷػ���̾
               07  LNK-BYO-TANDOKUKBN-BYOMEI
                                       PIC X(200).
      *        �ѻ�ǯ�������顼
               07  LNK-BYO-HAISIYMD-ERR
                                       PIC 9(08).
      *        ��̾�����ɣͣ��إ��顼
               07  LNK-BYO-BYOMEICD-MAX-ERR
                                       PIC 9(01).
      *        ����������Τߡ��������Ͻ票�顼
      *        ������̾������¸�ߥ��顼������������̾ʣ��¸��
               07  LNK-BYO-BYOMEICD-ERR
                                       PIC 9(01).
      *
      *    ������̾�����ɾ���
      *
             05  LNK-BYO-BYOMEI-MAX      PIC 9(02).
      *
             05  LNK-BYOMEICD-KENSAKU-AREA.
               07  LNK-BYO-BYOMEICD-G  OCCURS  21.
      *            ��̾
                   09  LNK-BYO-TBYOMEI PIC X(60).
      *            ��̾������
                   09  LNK-BYO-TBYOMEICD
                                       PIC X(07).
      *            �ܹ��襳����
                   09  LNK-BYO-TIKOSAKICD 
                                       PIC X(07).
      *            �ѻ�ǯ����
                   09  LNK-BYO-THAISIYMD 
                                       PIC X(08).
      *            ñ�Ȼ��Ѷػ߶�ʬ
                   09  LNK-BYO-TTANDOKUKBN 
                                       PIC 9(02).
      *            �������������
                   09  LNK-BYO-TTOKSKNCD
                                       PIC 9(02).
      *            ���³��襳����
                   09  LNK-BYO-TNANBYOCD
                                       PIC 9(02).
      *            �����ե饰
                   09  LNK-BYO-TUTAGAIFLG
                                       PIC X(01).
      *    ��̾�����ɺǽ�����
             05  LNK-BYO-HOSOKU-BYOMEICD-MAX
                                       PIC 9(02).
      *    ��­�����Ȥ��������̾��Ĺ��
             05  LNK-BYO-BYOMEI-LEN      PIC 9(03).
      *    ��­�����Ȥ��������̾
             05  LNK-BYO-HOSOKU-BYOMEI-ETC
                                       PIC X(200).
      *    ��­������
             05  LNK-BYO-HOSOKU-BYOMEI   PIC X(40).
      *???
      *
      *    ������̾�����ʣ����������
           03  LNK-BYOMEI-KENSAKU-MAX    PIC 9(02).
      *
           03  LNK-BYOMEI-AREA-OCC       OCCURS  6.
             05  LNK-BYOMEI-KENSAKU-AREA-O.
      *        ������̾������
               07  LNK-BYO-O-KHNBYOMEICD PIC X(07).
      *        �������̥�����
               07  LNK-BYO-O-KHNBUICD    PIC X(07)   OCCURS  3.
      *        ��̾�����ɿ�
               07  LNK-BYO-O-BYOMEICDSU  PIC 9(02).
      *        �����ե饰
               07  LNK-BYO-O-UTAGAIFLG   PIC X(01).
      *        �����ե饰
               07  LNK-BYO-O-MANSEIKBN   PIC 9(02).
      *        ���³��襳����
               07  LNK-BYO-O-NANBYOCD    PIC 9(02).
      *        ��̾ʸ����
               07  LNK-BYO-O-BYOMEIMOJI  PIC 9(03).
      *        ��̾�Խ��ե饰
               07  LNK-BYO-O-BYOMEIHENFLG
                                       PIC X(01).
      *        �ܹ��襳����
               07  LNK-BYO-O-IKOSAKICD   PIC X(07).
      *        �ܹ�����̾
               07  LNK-BYO-O-IKOSAKICD-BYOMEI
                                       PIC X(200).
      *        �ܹ�����̾������
               07  LNK-BYO-O-IKOSAKICD-INDEX
                                       PIC 9(02).
      *        ��̾���� 
               07  LNK-BYO-O-BYOMEISBT   PIC 9(01). 
      *        �쥻�ť��顼�ե饰
               07  LNK-BYO-O-RECEDENFLG  PIC X(01).
      *
      *    ������̾���顼����
             05  LNK-BYOMEI-KENSAKU-ERR-AREA-O.
      *        ����Ⱦ�ѥ����å����顼������������ʾ�
               07  LNK-BYO-O-KANACHK-ERR PIC 9(01).
      *        ñ�Ȼ��Ѷػߥ��顼
               07  LNK-BYO-O-TANDOKUKBN-ERR
                                       PIC 9(01).
      *        ñ�Ȼ��Ѷػ���̾
               07  LNK-BYO-O-TANDOKUKBN-BYOMEI
                                       PIC X(200).
      *        �ѻ�ǯ�������顼
               07  LNK-BYO-O-HAISIYMD-ERR
                                       PIC 9(08).
      *        ��̾�����ɣͣ��إ��顼
               07  LNK-BYO-O-BYOMEICD-MAX-ERR
                                       PIC 9(01).
      *        ����������Τߡ��������Ͻ票�顼
      *        ������̾������¸�ߥ��顼������������̾ʣ��¸��
               07  LNK-BYO-O-BYOMEICD-ERR
                                       PIC 9(01).
      *
      *    ������̾�����ɾ���
      *
             05  LNK-BYO-O-BYOMEI-MAX      PIC 9(02).
      *
             05  LNK-BYOMEICD-KENSAKU-AREA-O.
               07  LNK-BYO-O-BYOMEICD-G  OCCURS  21.
      *            ��̾
                   09  LNK-BYO-O-TBYOMEI PIC X(60).
      *            ��̾������
                   09  LNK-BYO-O-TBYOMEICD
                                       PIC X(07).
      *            �ܹ��襳����
                   09  LNK-BYO-O-TIKOSAKICD 
                                       PIC X(07).
      *            �ѻ�ǯ����
                   09  LNK-BYO-O-THAISIYMD 
                                       PIC X(08).
      *            ñ�Ȼ��Ѷػ߶�ʬ
                   09  LNK-BYO-O-TTANDOKUKBN 
                                       PIC 9(02).
      *            �������������
                   09  LNK-BYO-O-TTOKSKNCD
                                       PIC 9(02).
      *            ���³��襳����
                   09  LNK-BYO-O-TNANBYOCD
                                       PIC 9(02).
      *            �����ե饰
                   09  LNK-BYO-O-TUTAGAIFLG
                                       PIC X(01).
      *    ��̾�����ɺǽ�����
             05  LNK-BYO-O-HOSOKU-BYOMEICD-MAX
                                       PIC 9(02).
      *    ��­�����Ȥ��������̾��Ĺ��
             05  LNK-BYO-O-BYOMEI-LEN      PIC 9(03).
      *    ��­�����Ȥ��������̾
             05  LNK-BYO-O-HOSOKU-BYOMEI-ETC
                                       PIC X(200).
      *    ��­������
             05  LNK-BYO-O-HOSOKU-BYOMEI   PIC X(40).
