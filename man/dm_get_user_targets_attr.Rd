% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DM_trading_API_funs.R
\name{dm_get_user_targets_attr}
\alias{dm_get_user_targets_attr}
\title{Get User Targets with Attributes}
\usage{
dm_get_user_targets_attr(
  GameID = NULL,
  PriceFrom = NULL,
  PriceTo = NULL,
  Currency = NULL,
  Title = NULL,
  TargetID = NULL,
  Status = "TargetStatusActive",
  SortType = "UserTargetsSortTypeDefault",
  Offset = NULL,
  Limit = NULL,
  Cursor = NULL,
  is_active = NULL,
  attributes = FALSE
)
}
\description{
This function gets user targets with attributes.
 @param GameID Character. The game ID of the item to get.
 @param PriceFrom Numeric. The price from of the item to get.
 @param PriceTo Numeric. The price to of the item to get.
 @param Currency Character. The currency of the item to get.
 @param Title Character. The title of the item to get.
 @param TargetID Character. The target ID of the item to get.
 @param Status Character. The status of the item to get.
 @param SortType Character. The sort type of the item to get.
 @param Offset Character. The offset of the item to get.
 @param Limit Character. The limit of the item to get.
 @param Cursor Character. The cursor of the item to get.
 @param is_active Logical. The status of the item to get.
 @param attributes Logical. The attributes of the item to get.
 @return A tibble containing the response.
 @export
}
