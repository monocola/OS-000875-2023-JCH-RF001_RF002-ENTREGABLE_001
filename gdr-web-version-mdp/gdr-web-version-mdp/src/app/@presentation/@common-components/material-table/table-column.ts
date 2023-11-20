export interface TableColumn {
  name: string;
  dataKey: string;
  position?: 'right' | 'left' | 'center';
  isSortable?: boolean;
  width?: string;
  flagCombo?: boolean;
  flagCheck?: boolean;
  flagView?: boolean;
  flagColor?: string;
  settings?: string;
  innerHtml?: boolean;
  style?: string;
}
