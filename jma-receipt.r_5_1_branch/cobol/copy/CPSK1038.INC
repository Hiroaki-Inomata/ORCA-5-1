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
      *         �����ƥ�����ޥ���     (CPSK1038.INC)   *
      *         �ʣ���������                            *
      *         ���Ź԰ٰ�������������                *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-¿��Ǽ  04/07/05  �����ȯ����ˡ�ɲ�
      *  01.00.02    NACL-¿��Ǽ  06/07/11  �ģϽ��ɽ��¾�ɲ�
      *  03.04.00    NACL-¿��Ǽ  06/11/13  ��������̾��ɽ���ɲ�
      *  04.01.00    NACL-¿��Ǽ  07/10/25  ����ޡ�������ɽ���ɲ�
      *  04.01.00    NACL-¿��Ǽ  07/12/XX  ���������Ѵ�̵ͭ�ɲ�
      *                                     ��Ƴ���Ƚ�����ηٹ��ɲ�
      *  04.03.00    NACL-¿��Ǽ  08/07/04  ������ĢϢư�ɲ�
      *  04.03.00    NACL-¿��Ǽ  08/08/20  �쥻�ץȥ֥�ӥ塼��ʬ�ɲ�
      *  04.03.00    NACL-¿��Ǽ  08/11/04  ���𡦤�����Ģ�оݶ�ʬ�ɲ�
      *  04.06.00    NACL-¿��Ǽ  10/07/01  �����Ѵ���ʬ�ɲ�
      *  04.06.00    NACL-¿��Ǽ  10/08/03  ���⡦�ֶ�������ʬ�ɲ�
      *  04.06.00    NACL-¿��Ǽ  10/11/10  ȯ�����ѹ���ʬ�ɲ�
      *  04.07.00    NACL-¿��Ǽ  11/07/10  ��ϥӥꡦ���Ծ���ɽ����ʬ�ɲ�
      *  04.07.00    NACL-¿��Ǽ  12/01/25  ���ϥ����������طʿ��ɲ�
      *  04.07.00    NACL-¿��Ǽ  12/04/27  ��ǧ����̾��ɽ���ɲ�
      *  04.07.00    NACL-¿��Ǽ  12/11/21  �쥻�ץȥץ�ӥ塼�쥤�������ɲ�
      *  04.08.00    NACL-¿��Ǽ  16/06/XX  ���å�Ÿ����ʬ�ɲ�
      *  04.08.00    NACL-¿��Ǽ  17/07/XX  ������ˡ����̤���û���ʬ�ɲ�
      *  04.08.00    NACL-¿��Ǽ  18/09/XX  ���򼰥����Ȱ���ɽ���ɲ�
      ************************************************************
       01  SYS-1038-REC.
           03  SYS-1038-KEY.
               05  SYS-1038-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-1038-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-1038-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-1038-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-1038-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-1038-TBL.
      *���������ʬ
               05  SYS-1038-NYKNKNRKBN     PIC  X(01).
      *ʣ���ʤޤȤὸ��
               05  SYS-1038-KAGRPFLG       PIC  X(01).
      *�����ȯ����ˡ
               05  SYS-1038-HAKHOUFLG      PIC  X(01).
      *�ģϽ��ɽ��
               05  SYS-1038-DOSRYKBNFLG    PIC  X(02).
      *����ɽ������
               05  SYS-1038-WKSRYHYOFLG    PIC  X(01).
      *��������̾��ɽ��
               05  SYS-1038-FORMALFLG      PIC  X(01).
      *Ʊ���ƿǡ���������
               05  SYS-1038-TEISEIFLG      PIC  X(01).
      *�鲼�����������Ѵ�����
               05  SYS-1038-CHG310FLG      PIC  X(01).
      *���������������
               05  SYS-1038-TEIKINKBN      PIC  X(01).
      *���޾�����ȯ��
               05  SYS-1038-YKJCHKKBN      PIC  X(01).
      *����ޡ�������ɽ��
               05  SYS-1038-AKUSEIHYOFLG   PIC  X(01).
      *�������ż����Ѵ�����
               05  SYS-1038-CHGSRYSYU-FLG  PIC  X(01).
      *������Ģȯ��
               05  SYS-1038-OKUSURIKBN     PIC  X(01).
      *�쥻�ץȥ֥�ӥ塼
               05  SYS-1038-RESPRVFLG     PIC  X(01).
      *���𡦤�����Ģ�о�
               05  SYS-1038-YKJOKUKBN     PIC  X(01).
      *�����Ѵ���ʬ
               05  SYS-1038-HAIKIKBN      PIC  X(01).
      *���⡦�ֶ�������ʬ
               05  SYS-1038-NYKHNKKBN     PIC  X(01).
      *������ˡ����̤���û���ʬ
               05  SYS-1038-SEI20KSNKBN   PIC  X(01).
      *ȯ�����ѹ���ʬ
               05  SYS-1038-YMDHENKBN     PIC  X(01).
      *H24.4
      *��ǧ����̾��ɽ����ʬ
               05  SYS-1038-GENERICFLG    PIC  X(01).
      *
      *��ϥӥꡦ���Ծ���ɽ����ʬ
               05  SYS-1038-GMNHYOKBN-G.
                   07  SYS-1038-GMNHYOKBN     PIC  X(01)
                                              OCCURS   5.
      *���ϥ����������طʿ�
               05  SYS-1038-BGCOLORS       PIC  X(16).
      *���ϥ����������طʹԶ�ʬ
               05  SYS-1038-BGLINEKBN      PIC  X(01).
      *�Ķ������طʿ��ѹ�
               05  SYS-1038-M95BGCOLORS-G.
      *            ɸ��
                   07  SYS-1038-BGCOLORSJ0      PIC  X(16).
      *            �����ƥ������ѹ�
                   07  SYS-1038-BGCOLORSDAY     PIC  X(16).
      *            ���ֳ���ʬ
                   07  SYS-1038-BGCOLORSJ-G.
                       09 SYS-1038-BGCOLORSJ   PIC  X(16)
                                               OCCURS   8.
      *���顼�طʿ�
               05  SYS-1038-ERRCOLORS       PIC  X(16).
      *�ٹ��طʿ�
               05  SYS-1038-KEICOLORS       PIC  X(16).
      *�쥻�ץȥ֥�ӥ塼���쥤������
               05  SYS-1038-PRVLAYOUT       PIC  X(01).
      *���å�Ÿ����ʬ
               05  SYS-1038-SETALLKBN       PIC  X(01).
      *H28.10
      *��ϥӥꡦ���Ծ��󡦲��ɽ����ʬ��
               05  SYS-1038-GMNHYOKBN-6    PIC  X(01).
      *H29.7
      *������ˡ����̤���û���ʬ
               05  SYS-1038-SIN20KSNKBN    PIC  X(01).
      *H30.9
      *���򼰥����Ȱ�����ʬ
               05  SYS-1038-RECEKISAIFLG   PIC  X(01).
      *ͽ�� 
      ******   05                          PIC  X(488).
      ******   05  SYS-1038-YOBI           PIC  X(480).
      ****     05  SYS-1038-YOBI           PIC  X(454).
      ****     05  SYS-1038-YOBI           PIC  X(253).
      *****    05  SYS-1038-YOBI           PIC  X(213).
      ****     05  SYS-1038-YOBI           PIC  X(259).
               05  SYS-1038-YOBI           PIC  X(256).
      *ü���ɣ�
           03  SYS-1038-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-1038-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-1038-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-1038-UPYMD           PIC  X(08).
      *��������
           03  SYS-1038-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-1038-UPD-KEY.
               05  SYS-1038-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1038-UPD-KANRICD     PIC X(04).
               05  SYS-1038-UPD-KBNCD       PIC X(08).
               05  SYS-1038-UPD-STYUKYMD    PIC X(08).
               05  SYS-1038-UPD-EDYUKYMD    PIC X(08).