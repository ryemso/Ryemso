/**
 * Anonymized MongoDB aggregation examples from a product-analytics workflow.
 *
 * Collection and field names are intentionally generic.
 * This file demonstrates query structure only and is not production code.
 */

// 1. Count unique active users in a selected analysis window.
const activeUsersPipeline = [
  {
    $match: {
      createdAt: { $gte: START_DATE, $lt: END_DATE }
    }
  },
  {
    $group: {
      _id: "$userId"
    }
  },
  {
    $count: "activeUsers"
  }
];

// 2. Aggregate recommendation exposure by exposed profile.
const exposurePipeline = [
  {
    $match: {
      createdAt: { $gte: START_DATE, $lt: END_DATE }
    }
  },
  {
    $unwind: "$exposedProfileIds"
  },
  {
    $group: {
      _id: "$exposedProfileIds",
      exposureCount: { $sum: 1 }
    }
  },
  {
    $sort: { exposureCount: -1 }
  }
];

// 3. Calculate repeat-paying users.
const repeatPayersPipeline = [
  {
    $match: {
      status: "valid"
    }
  },
  {
    $group: {
      _id: "$userId",
      paymentCount: { $sum: 1 }
    }
  },
  {
    $match: {
      paymentCount: { $gte: 2 }
    }
  }
];

// 4. Example of joining profile attributes after exposure aggregation.
const exposureWithProfilePipeline = [
  ...exposurePipeline.slice(0, -1),
  {
    $lookup: {
      from: "user_profiles",
      localField: "_id",
      foreignField: "_id",
      as: "profile"
    }
  },
  {
    $unwind: "$profile"
  },
  {
    $project: {
      _id: 1,
      exposureCount: 1,
      exampleAttribute: "$profile.exampleAttribute"
    }
  }
];
