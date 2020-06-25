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
      *         û���ں߼������������                           *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  TANKI3-AREA.
           03  TANKI3-VAL.
      *        û�ꣳ�ʽ����̲�ݥꥰ��ե������ʷ��������֡ˡ�
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "97".
               05  FILLER          PIC X(09)   VALUE "190177610".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "20180331".
      *        û�ꣳ�ʽ����̲�ݥꥰ��ե������ʷ��������֡ˡˡ��������ܡ�
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "98".
               05  FILLER          PIC X(09)   VALUE "190177710".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "20180331".
      *        û�ꣳ�ʽ����̲�ݥꥰ��ե�������¿���������󥵡ˡ�
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "99".
               05  FILLER          PIC X(09)   VALUE "190177810".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "20180331".
      *        û�ꣳ�ʽ����̲�ݥꥰ��ե�������¿���������󥵡ˡˡ��������ܡ�
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "9A".
               05  FILLER          PIC X(09)   VALUE "190177910".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "20180331".
      *        û�ꣳ�ʽ����̲�ݥꥰ��ե������ʣ��ڤӣ��ʳ��ˡ�
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "9B".
               05  FILLER          PIC X(09)   VALUE "190178010".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "20200331".
      *        û�ꣳ�ʽ����̲�ݥꥰ��ե������ʣ��ڤӣ��ʳ��ˡˡ��������ܡ�
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "9C".
               05  FILLER          PIC X(09)   VALUE "190178110".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "20200331".
      *        û�ꣳ�ʾ�����ʪ����륮����ٸ�����
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "9D".
               05  FILLER          PIC X(09)   VALUE "190178210".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ�ʾ�����ʪ����륮����ٸ����ˡ��������ܡ�
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "9E".
               05  FILLER          PIC X(09)   VALUE "190178310".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ����Ω��������ˡ��
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "9F".
               05  FILLER          PIC X(09)   VALUE "190178410".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ����Ω��������ˡ�ˡ��������ܡ�
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "9G".
               05  FILLER          PIC X(09)   VALUE "190178510".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ�������ɼ�ѡ�����ͭ�����ڽ��ѡˡ�
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "9H".
               05  FILLER          PIC X(09)   VALUE "190178610".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "20180331".
      *        û�ꣳ�������ɼ�ѡ�����ͭ�����ڽ��ѡˡˡ��������ܡ�
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "9I".
               05  FILLER          PIC X(09)   VALUE "190178710".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "20180331".
      *        û�ꣳ�ʴ�������꺬�ɳ�����ѡ�
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "9J".
               05  FILLER          PIC X(09)   VALUE "190178810".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ�ʴ�������꺬�ɳ�����ѡˡ��������ܡ�
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "9K".
               05  FILLER          PIC X(09)   VALUE "190178910".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ�ʶ��ж����򴶿������ڽ��ѡ�ξ¦�ˡ�
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "9L".
               05  FILLER          PIC X(09)   VALUE "190179010".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ�ʶ��ж����򴶿������ڽ��ѡ�ξ¦�ˡˡ��������ܡ�
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "9M".
               05  FILLER          PIC X(09)   VALUE "190179110".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ�ʿ徽�κƷ��ѡ�����������������¾�Τ�Ρ�
      *        20140401-20160331
      *        û�ꣳ�ʿ徽�κƷ��ѡ�����������������¾����¦��
      *        20160401-99999999
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "9N".
               05  FILLER          PIC X(09)   VALUE "190179210".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ�ʿ徽�κƷ��ѡ�����������������¾�Τ�Ρˡ��������ܡ� 
      *        20140401-20160331
      *        û�ꣳ�ʿ徽�κƷ��ѡ�����������������¾����¦�ˡ��������ܡ� 
      *        20160401-99999999
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "9O".
               05  FILLER          PIC X(09)   VALUE "190179310".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ�ʿ徽�κƷ��ѡ�����������������¾��ξ¦�� 
      *        20160401-99999999
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "AY".
               05  FILLER          PIC X(09)   VALUE "190195910".
               05  FILLER          PIC X(08)   VALUE "20160401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ�ʿ徽�κƷ��ѡ�����������������¾��ξ¦�ˡ��������ܡ� 
      *        20160401-99999999
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "AZ".
               05  FILLER          PIC X(09)   VALUE "190196010".
               05  FILLER          PIC X(08)   VALUE "20160401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ�ʿ徽�κƷ��ѡ��徽��ǹ��ĥ��󥰡�˥��ʤ���
      *        20140701-20160331
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "AW".
               05  FILLER          PIC X(09)   VALUE "190182750".
               05  FILLER          PIC X(08)   VALUE "20140701".
               05  FILLER          PIC X(08)   VALUE "20160331".
      *        û�ꣳ�ʿ徽�κƷ��ѡ��徽��ǹ��ĥ��󥰡�˥��ʤ��ˡ��������ܡ�
      *        20140701-20160331
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "AX".
               05  FILLER          PIC X(09)   VALUE "190182850".
               05  FILLER          PIC X(08)   VALUE "20140701".
               05  FILLER          PIC X(08)   VALUE "20160331".
      *        û�ꣳ�ʿ徽�κƷ��ѡʴ������������ʤ����ˡ�  
      *        20140401-20160331
      *        û�ꣳ�ʿ徽�κƷ��ѡʴ������������ʤ�����¦�ˡ�
      *        20160401-99999999
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "9P".
               05  FILLER          PIC X(09)   VALUE "190179410".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "20180331".
      *        û�ꣳ�ʿ徽�κƷ��ѡʴ������������ʤ����ˡˡ��������ܡ�  
      *        20140401-20160331
      *        û�ꣳ�ʿ徽�κƷ��ѡʴ������������ʤ�����¦�ˡˡ��������ܡ�
      *        20160401-99999999
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "9Q".
               05  FILLER          PIC X(09)   VALUE "190179510".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "20180331".
      *        û�ꣳ�ʿ徽�κƷ��ѡʴ������������ʤ���ξ¦�ˡ�
      *        20160401-99999999
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "B0".
               05  FILLER          PIC X(09)   VALUE "190196110".
               05  FILLER          PIC X(08)   VALUE "20160401".
               05  FILLER          PIC X(08)   VALUE "20180331".
      *        û�ꣳ�ʿ徽�κƷ��ѡʴ������������ʤ���ξ¦�ˡˡ��������ܡ�
      *        20160401-99999999
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "B1".
               05  FILLER          PIC X(09)   VALUE "190196210".
               05  FILLER          PIC X(08)   VALUE "20160401".
               05  FILLER          PIC X(08)   VALUE "20180331".
      *        û�ꣳ����������Ŧ�нѡ�Ĺ�£����̤���ˡ�
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "9R".
               05  FILLER          PIC X(09)   VALUE "190179610".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ����������Ŧ�нѡ�Ĺ�£����̤���ˡˡ��������ܡ�
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "9S".
               05  FILLER          PIC X(09)   VALUE "190179710".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ�ʷ���Ū�����ȳ�ĥ�ѡ��������ѡ�
      *        20160401-99999999
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "B2".
               05  FILLER          PIC X(09)   VALUE "190196310".
               05  FILLER          PIC X(08)   VALUE "20160401".
               05  FILLER          PIC X(08)   VALUE "20200331".
      *        û�ꣳ�ʷ���Ū�����ȳ�ĥ�ѡ��������ѡˡ��������ܡ�
      *        20160401-99999999
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "B3".
               05  FILLER          PIC X(09)   VALUE "190196410".
               05  FILLER          PIC X(08)   VALUE "20160401".
               05  FILLER          PIC X(08)   VALUE "20200331".
      *        û�ꣳ�ʷ���Ū�����ȳ�ĥ�ѡ��������ѡʽ��ˡ�
      *        20160401-99999999
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "CF".
               05  FILLER          PIC X(09)   VALUE "190220910".
               05  FILLER          PIC X(08)   VALUE "20200401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ�ʷ���Ū�����ȳ�ĥ�ѡ��������ѡʽ��ˡˡ��������ܡ�
      *        20160401-99999999
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "CG".
               05  FILLER          PIC X(09)   VALUE "190221010".
               05  FILLER          PIC X(08)   VALUE "20200401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ�ʷ���Ū�����ȳ�ĥ�ѡ��������ѡ���������
      *        20160401-99999999
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "CH".
               05  FILLER          PIC X(09)   VALUE "190221110".
               05  FILLER          PIC X(08)   VALUE "20200401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ�ʷ���Ū�����ȳ�ĥ�ѡ��������ѡ��������ˡ��������ܡ�
      *        20160401-99999999
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "CI".
               05  FILLER          PIC X(09)   VALUE "190221210".
               05  FILLER          PIC X(08)   VALUE "20200401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ�ʲ�����̮���ѡ�ȴ���ڽ��ѡˡ�
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "9T".
               05  FILLER          PIC X(09)   VALUE "190179810".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ�ʲ�����̮���ѡ�ȴ���ڽ��ѡˡˡ��������ܡ�
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "9U".
               05  FILLER          PIC X(09)   VALUE "190179910".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ�ʲ�����̮���ѡʹŲ���ˡ�ˡ�
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "9V".
               05  FILLER          PIC X(09)   VALUE "190180010".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ�ʲ�����̮���ѡʹŲ���ˡ�ˡˡ��������ܡ�
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "9W".
               05  FILLER          PIC X(09)   VALUE "190180110".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ�ʲ�����̮���ѡʹ�̷�姽ѡˡ�
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "9X".
               05  FILLER          PIC X(09)   VALUE "190180210".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ�ʲ�����̮���ѡʹ�̷�姽ѡˡˡ��������ܡ�
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "9Y".
               05  FILLER          PIC X(09)   VALUE "190180310".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ���ͷ¥إ�˥���ѡʣ���̤���ˡ�
      *        20160401-99999999
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "B4".
               05  FILLER          PIC X(09)   VALUE "190196510".
               05  FILLER          PIC X(08)   VALUE "20160401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ���ͷ¥إ�˥���ѡʣ���̤���ˡˡ��������ܡ�
      *        20160401-99999999
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "B5".
               05  FILLER          PIC X(09)   VALUE "190196810".
               05  FILLER          PIC X(08)   VALUE "20160401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ���ͷ¥إ�˥���ѡʣ��аʾ壶��̤���ˡ�
      *        20160401-99999999
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "B6".
               05  FILLER          PIC X(09)   VALUE "190196610".
               05  FILLER          PIC X(08)   VALUE "20160401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ���ͷ¥إ�˥���ѡʣ��аʾ壶��̤���ˡˡ��������ܡ�
      *        20160401-99999999
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "B7".
               05  FILLER          PIC X(09)   VALUE "190196910".
               05  FILLER          PIC X(08)   VALUE "20160401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ���ͷ¥إ�˥���ѡʣ��аʾ壱����̤���ˡ�
      *        20160401-99999999
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "B8".
               05  FILLER          PIC X(09)   VALUE "190196710".
               05  FILLER          PIC X(08)   VALUE "20160401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ���ͷ¥إ�˥���ѡʣ��аʾ壱����̤���ˡˡ��������ܡ�
      *        20160401-99999999
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "B9".
               05  FILLER          PIC X(09)   VALUE "190197010".
               05  FILLER          PIC X(08)   VALUE "20160401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ���ͷ¥إ�˥���ѡʣ�����̤���ˡ�
      *        20140401-20160331
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "9Z".
               05  FILLER          PIC X(09)   VALUE "190180410".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "20160331".
      *        û�ꣳ���ͷ¥إ�˥���ѡʣ�����̤���ˡˡ��������ܡ�
      *        20140401-20160331
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "A0".
               05  FILLER          PIC X(09)   VALUE "190180510".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "20160331".
      *        û�ꣳ���ͷ¥إ�˥���ѡʣ����аʾ�ˡ�
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "A1".
               05  FILLER          PIC X(09)   VALUE "190180610".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ���ͷ¥إ�˥���ѡʣ����аʾ�ˡ��������ܡ�
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "A2".
               05  FILLER          PIC X(09)   VALUE "190180710".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ��ʢ�ж����ͷ¥إ�˥���ѡʣ���̤���ˡ�
      *        20160401-99999999
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "BA".
               05  FILLER          PIC X(09)   VALUE "190197110".
               05  FILLER          PIC X(08)   VALUE "20160401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ��ʢ�ж����ͷ¥إ�˥���ѡʣ���̤���ˡˡ��������ܡ�
      *        20160401-99999999
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "BB".
               05  FILLER          PIC X(09)   VALUE "190197410".
               05  FILLER          PIC X(08)   VALUE "20160401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ��ʢ�ж����ͷ¥إ�˥���ѡʣ��аʾ壶��̤���ˡ�
      *        20160401-99999999
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "BC".
               05  FILLER          PIC X(09)   VALUE "190197210".
               05  FILLER          PIC X(08)   VALUE "20160401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ��ʢ�ж����ͷ¥إ�˥���ѡʣ��аʾ壶��̤���ˡˡ��������ܡ�
      *        20160401-99999999
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "BD".
               05  FILLER          PIC X(09)   VALUE "190197510".
               05  FILLER          PIC X(08)   VALUE "20160401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ��ʢ�ж����ͷ¥إ�˥���ѡʣ��аʾ壱����̤���ˡ�
      *        20160401-99999999
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "BE".
               05  FILLER          PIC X(09)   VALUE "190197310".
               05  FILLER          PIC X(08)   VALUE "20160401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ��ʢ�ж����ͷ¥إ�˥���ѡʣ��аʾ壱����̤���ˡˡ��������ܡ�
      *        20160401-99999999
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "BF".
               05  FILLER          PIC X(09)   VALUE "190197610".
               05  FILLER          PIC X(08)   VALUE "20160401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ��ʢ�ж����ͷ¥إ�˥���ѡʣ�����̤���ˡ�
      *        20140401-20160331
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "A3".
               05  FILLER          PIC X(09)   VALUE "190180810".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "20160331".
      *        û�ꣳ��ʢ�ж����ͷ¥إ�˥���ѡʣ�����̤���ˡˡ��������ܡ�
      *        20140401-20160331
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "A4".
               05  FILLER          PIC X(09)   VALUE "190180910".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "20160331".
      *        û�ꣳ��ʢ�ж����ͷ¥إ�˥���ѡʣ����аʾ�ˡ�
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "A5".
               05  FILLER          PIC X(09)   VALUE "190181010".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ��ʢ�ж����ͷ¥إ�˥���ѡʣ����аʾ�ˡˡ��������ܡ�
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "A6".
               05  FILLER          PIC X(09)   VALUE "190181110".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ������Ū��Ĳ�ݥ꡼�ס�Ǵ���ڽ��ѣ����̤����
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "A7".
               05  FILLER          PIC X(09)   VALUE "190181210".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ������Ū��Ĳ�ݥ꡼�ס�Ǵ���ڽ��ѣ����̤���ˡ��������ܡ�
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "A8".
               05  FILLER          PIC X(09)   VALUE "190181310".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ������Ū��Ĳ�ݥ꡼�ס�Ǵ���ڽ��ѣ�����ʾ��
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "A9".
               05  FILLER          PIC X(09)   VALUE "190181410".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ������Ū��Ĳ�ݥ꡼�ס�Ǵ���ڽ��ѣ�����ʾ�ˡ��������ܡ�
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "AA".
               05  FILLER          PIC X(09)   VALUE "190181510".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ�ʼ��˼�ѡʹŲ���ˡ�ʻ��ʳ�����ˡ�ˡˡ�
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "AB".
               05  FILLER          PIC X(09)   VALUE "190181610".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ�ʼ��˼�ѡʹŲ���ˡ�ʻ��ʳ�����ˡ�ˡˡˡ��������ܡ�
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "AC".
               05  FILLER          PIC X(09)   VALUE "190181710".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ���γ��׷��ȿա�Ǣ�ɷ����˺սѡ�
      *        20160401-99999999
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "BG".
               05  FILLER          PIC X(09)   VALUE "190197710".
               05  FILLER          PIC X(08)   VALUE "20160401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ���γ��׷��ȿա�Ǣ�ɷ����˺սѡˡ��������ܡ�
      *        20160401-99999999
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "BH".
               05  FILLER          PIC X(09)   VALUE "190197810".
               05  FILLER          PIC X(08)   VALUE "20160401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ�ʻҵ�������������ڽ��ѡ�
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "AD".
               05  FILLER          PIC X(09)   VALUE "190181810".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ�ʻҵ�������������ڽ��ѡˡ��������ܡ�
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "AE".
               05  FILLER          PIC X(09)   VALUE "190181910".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ�ʻҵܶ����ҵܶڼ�Ŧ�нѡ�
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "AF".
               05  FILLER          PIC X(09)   VALUE "190182010".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "20200331".
      *        û�ꣳ�ʻҵܶ����ҵܶڼ�Ŧ�нѡˡ��������ܡ�
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "AG".
               05  FILLER          PIC X(09)   VALUE "190182110".
               05  FILLER          PIC X(08)   VALUE "20140401".
               05  FILLER          PIC X(08)   VALUE "20200331".
      *        û�ꣳ�ʥ���ޥʥ��դˤ��������������š�
      *        20160401-99999999
               05  FILLER          PIC X(01)   VALUE "0".
               05  FILLER          PIC X(02)   VALUE "BI".
               05  FILLER          PIC X(09)   VALUE "190197910".
               05  FILLER          PIC X(08)   VALUE "20160401".
               05  FILLER          PIC X(08)   VALUE "99999999".
      *        û�ꣳ�ʥ���ޥʥ��դˤ��������������šˡ��������ܡ�
      *        20160401-99999999
               05  FILLER          PIC X(01)   VALUE "1".
               05  FILLER          PIC X(02)   VALUE "BJ".
               05  FILLER          PIC X(09)   VALUE "190198010".
               05  FILLER          PIC X(08)   VALUE "20160401".
               05  FILLER          PIC X(08)   VALUE "99999999".
           03  TANKI3-R  REDEFINES TANKI3-VAL.
               05  TANKI3-OCC      OCCURS      74.
                   07  TANKI3-LIFEKBN          PIC X(01).
                   07  TANKI3-TOKCD            PIC X(02).
                   07  TANKI3-TANKICD          PIC X(09).
                   07  TANKI3-YUKOSTYMD        PIC X(08).
                   07  TANKI3-YUKOEDYMD        PIC X(08).
       01  TANKI3-MAX                          PIC 9(03)   VALUE 74.